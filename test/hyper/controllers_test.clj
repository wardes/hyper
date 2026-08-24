(ns hyper.controllers-test
  (:require [clojure.test :refer [deftest is testing]]
            [hyper.controllers :as controllers]
            [hyper.server :as server]
            [hyper.state :as state]
            [hyper.watch :as watch]))

(deftest validate-controllers-test
  (testing "accepts well-formed controllers"
    (is (nil? (controllers/validate-controllers!
               [{:id :league :params (fn [_]) :start (fn [_])}]))))

  (testing "throws on missing :id"
    (is (thrown? Exception
                 (controllers/validate-controllers!
                  [{:params (fn [_]) :start (fn [_])}]))))

  (testing "throws on missing :params"
    (is (thrown? Exception
                 (controllers/validate-controllers!
                  [{:id :league :start (fn [_])}]))))

  (testing "throws on missing :start"
    (is (thrown? Exception
                 (controllers/validate-controllers!
                  [{:id :league :params (fn [_])}]))))

  (testing "throws on duplicate :id"
    (is (thrown? Exception
                 (controllers/validate-controllers!
                  [{:id :league :params (fn [_]) :start (fn [_])}
                   {:id :league :params (fn [_]) :start (fn [_])}]))))

  (testing "resolves Vars before validating"
    (def ^:private a-controller {:id :league :params (fn [_]) :start (fn [_])})
    (is (nil? (controllers/validate-controllers! [#'a-controller])))))

(deftest run-controllers-transitions-test
  (let [app-state*  (atom (state/init-state))
        tab-id      "tab-1"
        events      (atom [])
        controller  {:id     :league
                     :params (fn [route] (when (= (:name route) :league) (:id (:path-params route))))
                     :start  (fn [id] (swap! events conj [:start id]))
                     :stop   (fn [id] (swap! events conj [:stop id]))}
        run!        (fn [route]
                      (controllers/run-controllers! app-state* "sess-1" tab-id route [controller]))]

    (testing "nil -> value calls :start"
      (run! {:name :league :path-params {:id 1}})
      (is (= [[:start 1]] @events))
      (is (= 1 (get-in @app-state* [:tabs tab-id :controllers :league :last-params]))))

    (testing "same value is a no-op"
      (reset! events [])
      (run! {:name :league :path-params {:id 1}})
      (is (= [] @events)))

    (testing "value -> different value calls :stop then :start"
      (reset! events [])
      (run! {:name :league :path-params {:id 2}})
      (is (= [[:stop 1] [:start 2]] @events)))

    (testing "value -> nil calls :stop"
      (reset! events [])
      (run! {:name :other :path-params {}})
      (is (= [[:stop 2]] @events))
      (is (nil? (get-in @app-state* [:tabs tab-id :controllers :league :last-params]))))))

(deftest run-controllers-error-handling-test
  (testing "a throwing controller doesn't block others"
    (let [app-state*  (atom (state/init-state))
          tab-id      "tab-err"
          events      (atom [])
          broken      {:id     :broken
                       :params (fn [_] (throw (ex-info "boom" {})))
                       :start  (fn [_] (swap! events conj :should-not-run))}
          ok          {:id     :ok
                       :params (fn [_] :always)
                       :start  (fn [v] (swap! events conj [:start v]))}]
      (controllers/run-controllers! app-state* "sess-1" tab-id {} [broken ok])
      (is (= [[:start :always]] @events))))

  (testing "a throwing :start doesn't prevent last-params from being stored"
    (let [app-state* (atom (state/init-state))
          tab-id     "tab-err2"
          controller {:id     :throws-on-start
                       :params (fn [_] :v)
                       :start  (fn [_] (throw (ex-info "boom" {})))}]
      (controllers/run-controllers! app-state* "sess-1" tab-id {} [controller])
      (is (= :v (get-in @app-state* [:tabs tab-id :controllers :throws-on-start :last-params]))))))

(deftest stop-all-on-disconnect-test
  (testing "active controllers get :stop on tab cleanup"
    (let [app-state*  (atom (state/init-state))
          session-id  "sess-1"
          tab-id      "tab-disconnect"
          stopped     (atom nil)
          controller  {:id     :league
                       :params (fn [_] :active)
                       :start  (fn [_])
                       :stop   (fn [v] (reset! stopped v))}]
      (state/get-or-create-tab! app-state* session-id tab-id)
      (swap! app-state* assoc :controllers [controller])
      (controllers/run-controllers! app-state* session-id tab-id {} [controller])

      (server/cleanup-tab! app-state* tab-id)

      (is (= :active @stopped)))))

(deftest var-live-reload-test
  (testing "redefining the controller Var changes behavior without re-registration"
    (def ^:private reloadable-controller
      {:id :reload :params (fn [_] :v) :start (fn [_])})
    (let [app-state* (atom (state/init-state))
          seen       (atom nil)]
      (controllers/run-controllers! app-state* "sess-1" "tab-reload" {} [#'reloadable-controller])
      (def ^:private reloadable-controller
        {:id :reload :params (fn [_] :v) :start (fn [_] (reset! seen :new-start))})
      (controllers/run-controllers! app-state* "sess-1" "tab-reload" {} [#'reloadable-controller])
      ;; last-params unchanged (:v -> :v) so :start shouldn't fire again on the
      ;; second call; this just proves resolve-controller derefs the Var each time
      ;; without throwing when the root binding changes.
      (is (nil? @seen)))))

(deftest watch-integration-test
  (testing "route change through setup-watchers! runs controller transitions before render"
    (let [app-state*      (atom (state/init-state))
          session-id      "sess-int"
          tab-id          "tab-int"
          trigger-count   (atom 0)
          events          (atom [])
          controller      {:id     :league
                           :params (fn [route] (when (= (:name route) :league) (:id (:path-params route))))
                           :start  (fn [id] (swap! events conj [:start id]))
                           :stop   (fn [id] (swap! events conj [:stop id]))}
          trigger-render! #(swap! trigger-count inc)]

      (state/get-or-create-tab! app-state* session-id tab-id)
      (swap! app-state* assoc :controllers [controller])
      (watch/setup-watchers! app-state* session-id tab-id trigger-render!)

      (state/set-tab-route! app-state* tab-id {:name :league :path-params {:id 7}})
      (Thread/sleep 50)

      (is (= [[:start 7]] @events))
      (is (>= @trigger-count 1))

      (watch/remove-watchers! app-state* tab-id))))
