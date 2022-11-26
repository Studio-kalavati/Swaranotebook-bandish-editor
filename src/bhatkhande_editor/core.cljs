(ns bhatkhande-editor.core
  (:require
   [reagent.dom :as rdom]
   [re-frame.core :as re-frame]
   [breaking-point.core :as bp]
   [bhatkhande-editor.events :as events]
   [bhatkhande-editor.routes :as routes]
   [bhatkhande-editor.views :as views]
   [bhatkhande-editor.db :as db]
   [bhatkhande-editor.config :as config]
   ["firebase/app" :as fbapp]
   ["firebase/auth" :as fbauth]
   [com.degel.re-frame-firebase :as firebase]
   )
  (:require-macros [adzerk.env :as env]))


(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [views/main-panel] root-el)))

(defonce firebase-app-info
  {:apiKey db/apiKey
   :authDomain db/authDomain
   :projectId db/projectId
   :messagingSenderId db/messagingSenderId
   :appId db/appId
   :storageBucket db/storageBucket
   })

(defn init []
  (re-frame/dispatch-sync [::events/initialize-db])
  (firebase/init :firebase-app-info  firebase-app-info
                 :firestore-settings     {
                                          :timestampsInSnapshots true
                                          }
                 :get-user-sub           [::events/user]
                 :set-user-event         [::events/set-user]
                 :default-error-handler  [::events/firebase-error])

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
