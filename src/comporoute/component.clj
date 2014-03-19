(ns comporoute.component
  "Component providing facilities for independent components to add
  routes to one handler."
  (:require [comporoute.core :as comporoute]
            [com.stuartsierra.component :as component]
            [clojure.string :as str]))

(def ^:private inst*
  "Instance of currently running router component."
  nil)

(defn- parse-root-path
  [path]
  (-> path
      (str/split #"/")
      (->> (remove empty?))
      first str))

(defn- assoc-app-spec
  [app-specs root-path app-spec]
  (when-let [existing (get app-specs root-path)]
    (throw (IllegalArgumentException. (str "Can't add app under used root-path: "
                                           root-path))))
  (assoc app-specs root-path app-spec))

(defn- wrap-app-middleware
  [app-specs handler-fn]
  (let [wrap-middleware
        (memoize ;; TODO: this is still sloppy because requests could
                 ;; be made with random paths to blow up the memoize atom.
         (fn [root-path]
           (let [app-spec (or (get app-specs root-path)
                              (get app-specs ""))
                 middleware (:middleware app-spec)]
             (cond-> handler-fn
               middleware middleware))))]
    (fn [request]
      ((-> (:uri request)
           parse-root-path
           wrap-middleware)
       request))))

(defn- recompile-handler
  [router-state]
  (let [app-specs (:app-specs router-state)
        routes (->> app-specs
                    (map (fn [[prefix app-spec]]
                           [prefix (:routes app-spec)]))
                    comporoute/build-routes)
        handler (->> routes
                     comporoute/router
                     (wrap-app-middleware app-specs))]
    (assoc router-state
      :routes routes
      :handler handler)))

(defrecord Router []
  component/Lifecycle
  (start [this]
    (let [state (atom nil)
          inst (assoc this
                 :handle (fn [request]
                           ((:handler @state) request))
                 :state state)]
      (alter-var-root #'inst* (constantly inst))))
  (stop [this] this))

(defn router
  "Create a router component. Once started, the request handler
  function is available under :handle."
  []
  (->Router))

(defn add-app*
  [router root-path app-spec]
  (swap! (:state router)
         #(-> %
              (update-in [:app-specs]
                         assoc-app-spec (parse-root-path root-path) app-spec)
              (recompile-handler))))

(defn remove-app*
  [this root-path]
  (swap! (:state router)
         #(-> %
              (update-in [:app-specs]
                         dissoc (parse-root-path root-path))
              (recompile-handler))))

(defn add-app!
  "Add an app to the (started) system router. An app spec is a
  hash-map of keys

  :middleware One middleware function that will be invoked with the
  app handler and be called when a uri is requested that begins with
  the apps prefix.

  :routes A comporoute route vector

  All routes of the app will be available under prefix. Prefixes must
  be unique among used apps."
  [prefix app-spec]
  (add-app* inst* prefix app-spec))

(defn remove-app!
  [prefix]
  (remove-app* inst* prefix))

(defn routes
  "Access compiled routes to use core API on them."
  []
  (get @(:state inst*) :routes))

(defn url-for
  "Short for (core/url-for (routes) ...)"
  [& args]
  (apply comporoute/url-for (routes) args))
