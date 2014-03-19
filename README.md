# comporoute

Composable routing designed for systems with independent components
providing routes.

Provides facilities to build a clout route handler from one or many
independent specifications with path-parameterization features and
reverse routing known from similar libs.

Provides a runtime component for systems that offers adding and
removing routes dynamically in system level operations like stopping a
component without stopping the server and other components. Also,
components using comporoute wrap their individual middleware.

# Influences

Most features for the route-spec DSL are heavily inspired by the
caribou/polaris project.

# Status

Unreleased alpha codebase.

Several decisions have not yet been made and minor technical
challenges have to be overcome.

This readme will be updated with a walkthrough and a license when the
project reaches a more solid state.