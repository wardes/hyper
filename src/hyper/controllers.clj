(ns hyper.controllers
  "Kee-frame-style route controllers for hyper applications.

   A controller is a map {:id :start :params :stop}. :params derives a
   value from the tab's current route; when that value changes, :start/:stop
   fire per the standard nil-transition rules (see run-controllers!)."
  (:require [hyper.context :as context]
            [taoensso.telemere :as t]))

(defn resolve-controller
  "Resolve a controller entry to its realized map, dereferencing Vars for
   REPL live-reload."
  [c]
  (if (var? c) @c c))

(defn validate-controllers!
  "Validate a :controllers option at create-handler setup time.
   Throws ex-info if any resolved controller is missing :id/:params/:start,
   or if two controllers share an :id."
  [controllers]
  (let [resolved (mapv resolve-controller controllers)]
    (doseq [{:keys [id params start]} resolved]
      (when-not id
        (throw (ex-info "Controller is missing :id" {:controller (first resolved)})))
      (when-not params
        (throw (ex-info "Controller is missing :params" {:hyper/controller-id id})))
      (when-not start
        (throw (ex-info "Controller is missing :start" {:hyper/controller-id id}))))
    (let [dupes (->> resolved (map :id) frequencies (keep (fn [[id n]] (when (> n 1) id))) seq)]
      (when dupes
        (throw (ex-info "Duplicate controller :id" {:hyper/duplicate-ids dupes}))))
    nil))

(defn- with-controller-context
  "Run f with context/*request* bound so cursor constructors work inside
   :start/:stop bodies, catching and logging any throw so one controller's
   failure can't block others or break navigation/rendering."
  [error-id session-id tab-id app-state* f & args]
  (t/catch->error! {:id error-id :catch-val nil}
                   (binding [context/*request* {:hyper/session-id session-id
                                                :hyper/tab-id     tab-id
                                                :hyper/app-state  app-state*
                                                :hyper/router     (:router @app-state*)}]
                     (apply f args))))

(defn run-controllers!
  "Evaluate :params for every controller against `route`, compare to the
   tab's stored last-params, and run :stop/:start transitions as needed.
   Stores each controller's new params under
   [:tabs tab-id :controllers id :last-params]."
  [app-state* session-id tab-id route controllers]
  (doseq [{:keys [id params start stop]} (map resolve-controller controllers)]
    (let [state-path [:tabs tab-id :controllers id :last-params]
          prev       (get-in @app-state* state-path)
          curr       (t/catch->error! {:id :hyper.error/controller-params :catch-val nil} (params route))]
      (when (not= prev curr)
        (when (and (some? prev) stop)
          (with-controller-context :hyper.error/controller-stop
            session-id tab-id app-state* stop prev))
        (when (some? curr)
          (with-controller-context :hyper.error/controller-start
            session-id tab-id app-state* start curr)))
      (swap! app-state* assoc-in state-path curr)))
  nil)

(defn stop-all!
  "Run :stop for every controller currently active (non-nil last-params)
   on a tab. Called during tab cleanup, before tab state is wiped, so
   controller-held resources aren't leaked on disconnect."
  [app-state* tab-id]
  (let [session-id  (get-in @app-state* [:tabs tab-id :session-id])
        controllers (into {} (map (juxt :id identity) (map resolve-controller (:controllers @app-state*))))
        active      (get-in @app-state* [:tabs tab-id :controllers])]
    (doseq [[id {:keys [last-params]}] active]
      (when (some? last-params)
        (when-let [{:keys [stop]} (get controllers id)]
          (when stop
            (with-controller-context :hyper.error/controller-stop
              session-id tab-id app-state* stop last-params))))))
  nil)
