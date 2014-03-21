(ns comporoute.core
  (:require [comporoute.util :as util]
            [clout.core :as clout]
            [ring.util.codec :as codec]
            [clojure.string :as str]))

(defn- sanitize-method
  [method]
  (-> (if (and (string? method)
               (empty? method))
        :ALL
        method)
      name
      str/lower-case
      keyword))

(defn- resolve-action-symbol
  [action]
  (doto (namespace action)
    (when-not (throw (IllegalArgumentException.
                      (str "Must be namespace qualified: " action))))
    (-> symbol require))
  (resolve action))

(defn- sanitize-action-subspec
  [subspec middleware]
  (cond->
   (doto (cond
          (fn? subspec) subspec
          (var? subspec) subspec
          (symbol? subspec) (resolve-action-symbol subspec))
     (when-not (throw (IllegalArgumentException.
                       (str "Invalid action-subspec: " subspec)))))
   middleware
   middleware))

(defn- sanitize-action-spec
  "Convert action-spec to a hash-map request-method->action."
  [action-spec middleware]
  (if (map? action-spec)
    (reduce-kv #(assoc %1
                  (sanitize-method %2)
                  (sanitize-action-subspec %3 middleware)) {} action-spec)
    (recur {:ALL action-spec} middleware)))

(defn- add-optional-slash-to-route
  [route]
  (update-in route [:re]
             (fn [re]
               (re-pattern (str re "/?")))))

(defn- compile-path
  [path]
  (-> path
      clout/route-compile
      add-optional-slash-to-route))

(defn- sanitize-user-specs
  [middleware specs] ;; Nil may be specified as middleware and will be
                     ;; interpreted as identity (but wrapping the
                     ;; identity-fn is avoided)
  (cond
   (coll? (first specs))
   (not-empty
    (mapcat (partial sanitize-user-specs middleware) specs))
   
   (seq specs)
   (let [[obj0 obj1 obj2] specs
         idents (cond (keyword? obj1) #{obj1}
                      (set? obj1) obj1)]
     (if (string? obj0)
       [(merge {:path (util/compose-path obj0)}
               (if idents
                 {:idents idents
                  :action-spec (sanitize-action-spec obj2 middleware)
                  :sub-specs (->> specs
                                  (drop 3)
                                  (sanitize-user-specs middleware))}
                 (if (sequential? obj1)
                   {:sub-specs (->> specs
                                    rest
                                    (sanitize-user-specs middleware))}
                   {:action-spec (sanitize-action-spec obj1 middleware)
                    :sub-specs (->> specs
                                    (drop 2)
                                    (sanitize-user-specs middleware))})))]
       ;; Wrap the middleware around all sub-specs
       (->> (rest specs)
            (sanitize-user-specs
             ;; middleware is specified like actions
             (cond->> (sanitize-action-subspec obj0 nil)
                      middleware
                      (comp middleware))))))))

(defn- merge-specs-by-path
  "If multiple routes are specified with the same path on one level,
  merge them ontop of each other."  
  [specs]
  (->> specs
       (map :path)
       distinct
       (map (group-by :path specs))
       (map (fn [specs]
              (-> (reduce #(-> %1
                               (update-in [:idents]
                                          (fnil into #{}) (:idents %2))
                               (update-in [:action-spec]
                                          merge (:action-spec %2))
                               (update-in [:sub-specs]
                                          concat (:sub-specs %2)))
                          specs)
                  (update-in [:sub-specs]
                             #(some-> % merge-specs-by-path)))))))

(defn- compile-specs
  "Generate full sub-paths and compile Clout routes."
  [root-path specs]
  (letfn [(step [{:keys [idents path action-spec sub-specs] :as spec}]
            (let [sub-path (util/compose-path root-path path)]
              (-> []
                  (cond-> action-spec
                          (conj (assoc spec
                                  :compiled-path (compile-path sub-path)
                                  :full-path sub-path)))
                  (into (compile-specs sub-path sub-specs)))))]
    (mapcat step specs)))

(defn- to-routes
  ([user-specs] (to-routes "/" user-specs))
  ([root-path user-specs]
     (->> user-specs
          (sanitize-user-specs nil)
          (merge-specs-by-path)
          (compile-specs root-path))))

(defn- match-in-routes
  [request routes]
  (reduce (fn [_ route]
            (when-let [match (clout/route-matches (:compiled-path route)
                                                  request)]
              (reduced [route match]))) nil routes))

(defn- assoc-ident-lookup
  [acc route]
  (let [idents (:idents route)]
    (doseq [ident idents]
      (when-let [existing (get acc ident)]
        (throw
         (IllegalArgumentException.
          (str
           "Can't add route with ident: " ident \newline
           "Existing spec: " \tab existing
           "At path: " \tab (:full-path route))))))
    (merge acc
           (zipmap idents (repeat route)))))

(defn- make-lookup-tables
  [routes]
  (reduce (fn [acc route]
            (-> acc
                (update-in [:by-ident]
                           assoc-ident-lookup route)))
          {:routes routes} routes))


(defn- build-routes
  [& specs]
  (-> specs to-routes make-lookup-tables))

(defn- build-query-string
  [opts-map]
  (str (some->> opts-map
                (mapv (fn [[k v]] (str (codec/url-encode (name k))
                                       "="
                                       (codec/url-encode v))))
                (str/join "&")
                not-empty
                (str "?"))))

(defn- url-for*
  ([routes ident] (url-for* routes ident {}))
  ([routes ident params] (url-for* routes ident params {}))
  ([routes ident params opts]
     (let [path (get-in routes [:by-ident (keyword ident) :full-path])
           [sub-dirs params-left]
           (->> (str/split path #"/")
                (filter seq)
                (reduce (fn [[sub-dirs params-left] sub-dir]
                          (if (= (first sub-dir) \:)
                            (let [kw (keyword (subs sub-dir 1))]
                              [(conj sub-dirs (or (str (get params-left kw))
                                                  sub-dir))
                               (dissoc params-left kw)])
                            [(conj sub-dirs sub-dir)
                             params-left]))
                        [[] params]))]
       (cond-> (apply util/compose-path sub-dirs)
         (not (:no-query opts))
         (str (build-query-string params-left))))))

(def ^:private default-method-not-allowed
  (constantly
   {:status 405
    :body "Method not allowed."}))

(def ^:private default-page-not-found
  (constantly {:status 404
                   :body "Page not found."}))

;; API
(defn url-for
  "Reconstruct url for route at ident based on parameters in params
  (optional). opts may specify the following settings in a hash-map

  :no-query When set to logical true, don't append a query-string like
  ?unmatched-param=its-val for params that could not be used to name
  sub-dirs in the route."
  ([req ident]
     (url-for* (::routes req) ident))
  ([req ident params]
     (url-for* (::routes req) ident params))
  ([req ident params opts]
     (url-for* (::routes req) ident params opts)))

(defn idents
  "Return idents of the matched route for the ring request map."
  [req]
  (::route-idents req))

(defn router
  "Create a comporoute router

  A spec must be a vector of either

  [path-spec ident action-spec & specs*] or

  [path-spec action-spec] or

  [path-spec & specs*]

  [middleware & specs*]


  path-spec is a string specifying the parent directory (by default
  \"/\") of nested specs. It may contain parameterizable sub-paths
  such as \"/home/profiles/:username/\". These can be resolved under
  :params in the request when it is passed to an action.

  Idents can used for reverse-routing (url-for). They must be keywords
  and are optional. Multiple idents for one route may be specified in
  a set instead of a single keyword. The same idents may be used
  exclusively in specs that have identical paths and reside on one
  level.

  action-spec is either:

  - a function that will be invoked for all requests that match the
    path

  - a namespace qualified symbol that will be resolved before this
    function returns. require will be invoked on its namespace

  - a var that can be resolved to a function at request time

  - a hash-map mapping request-methods to one of those where :all may
    be used as to provide a fallback handler if no handler is
    specified for a requests method.

  middleware:

  A function of one argument may be specified (directly, as symbol or
  var like action-specs) and is invoked with each handler of deeper
  levels. This has the benefit that dispatched parameters and the
  route ident are available to the middleware at request time. Nesting
  multiple middlewares is fully supported.

  (During routing, routes will be matched in the order they are
  provided, regardless of their hierachy level, unless they are on the
  same level and have the same path in which case their action-specs
  and idents are merged)

  * multiple specs may be grouped in an extra vector


  A map with resolved parameters is merged onto :params in requests
  passed to resolved actions, routes can be found under :routes.

  Custom fallback handlers may be added under the kw-args
  :method-not-allowed and :page-not-found"
  [specs & {:keys [method-not-allowed page-not-found]}]
  (let [method-not-allowed (or method-not-allowed
                               default-method-not-allowed)
        page-not-found (or page-not-found
                           default-page-not-found)
        {:keys [routes] :as compiled} (build-routes specs)]
    (fn [{method :request-method :as request}]
      (let [[route match]
            (match-in-routes request routes)
            {:keys [action-spec]} route
            action (or (get action-spec (or method
                                            (sanitize-method :GET)))
                       (get action-spec (sanitize-method :ALL)))]
        (if match
          (if action
            (-> request
                (assoc ::routes compiled
                       ::route-idents (:idents route))
                (update-in [:params] merge match)
                action)
            (method-not-allowed request))
          (page-not-found request))))))
