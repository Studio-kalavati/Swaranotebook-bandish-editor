(ns bhatkhande-editor.routes
  (:require
   [bidi.bidi :as bidi]
   [pushy.core :as pushy]
   [re-frame.core :as re-frame :refer [dispatch]]
   [cemerick.url :as curl]
   [re-com.core :as re-com :refer [throbber
                                   modal-panel]]
   [bhatkhande-editor.events :as events]))

(defmulti panels identity)
(defmethod panels :default []
  [:div
   [modal-panel
    :backdrop-color "#f83600"
    :child [:div {:class "popup"
                  :style {:overflow-y :scroll
                          :max-height "80vh"}}
            [throbber :size :large]]]])

(def routes
  (atom
   ["/app/"
    {"" :home
     "index.html" :home
     "list" :list-comps
     "view/" {[:path "/" :id]:load}}]))

(defn match-route-with-query-params
  [route path & {:as options}]
  (let [query-params (->> (:query (curl/url path))
                          (map (fn [[k v]] [(keyword k) v]))
                          (into {}))]
    (-> (bidi/match-route* route path options)
        (assoc :query-params query-params))))

(defn parse
  [url]
  (match-route-with-query-params @routes url))

(defn url-for
  [& args]
  (apply bidi/path-for (into [@routes] args)))

(defn dispatch-route
  [route]
  (let [hand (:handler route)
        panel (keyword (str (name hand) "-panel"))]
    (when-let [id  (-> route :route-params :id)]
      (dispatch [::events/set-mode :edit])
      (dispatch [::events/get-bandish-json
                          {:path (-> route :route-params :path)
                           :id id}]))
    (when-let [qp (:query-params route)]
      (dispatch [::events/set-query-params qp]))

    (dispatch [::events/set-active-panel panel])))

(defonce history
  (pushy/pushy dispatch-route parse))

(defn navigate!
  [handler]
  (pushy/set-token! history (url-for handler)))

(defn start!
  []
  (pushy/start! history))

(re-frame/reg-fx
  :navigate
  (fn [handler]
    (navigate! handler)))

(re-frame/reg-fx
 :navigate-to-comp
 (fn [[path title]]
   (pushy/set-token! history
                     (bidi/path-for @routes :load :path path :id title))))
