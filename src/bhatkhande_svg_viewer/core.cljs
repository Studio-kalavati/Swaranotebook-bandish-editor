(ns bhatkhande-svg-viewer.core
  (:require
   [reagent.dom :as rdom]
   [re-frame.core :as re-frame]
   [breaking-point.core :as bp]
   [bhatkhande-svg-viewer.events :as events]
   [bhatkhande-svg-viewer.routes :as routes]
   [bhatkhande-svg-viewer.views :as views]
   [bhatkhande-svg-viewer.config :as config]
   ))


(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [views/main-panel] root-el)))

(defn init []
  (routes/start!)
  (re-frame/dispatch-sync [::events/initialize-db])
  (re-frame/dispatch-sync [::bp/set-breakpoints
                           {:breakpoints [:mobile
                                          768
                                          :tablet
                                          992
                                          :small-monitor
                                          1200
                                          :large-monitor]
                            :debounce-ms 166}])
  (dev-setup)
  (mount-root))
