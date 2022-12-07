(ns bhatkhande-editor.routes
  (:require
   [bidi.bidi :as bidi]
   [pushy.core :as pushy]
   [re-frame.core :as re-frame]
[re-com.core :as re-com :refer [
                                   border
                                   box
                                   throbber
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
   ["/" {"" :home
         "view/" {[:path "/" :id]:load}}]))

(defn parse
  [url]
  (bidi/match-route @routes url))

(defn url-for
  [& args]
  (apply bidi/path-for (into [@routes] args)))

(defn dispatch
  [route]
  (let [panel (keyword (str (name (:handler route)) "-panel"))]
    (when-let [id  (-> route :route-params :id)]
      (re-frame/dispatch [::events/get-bandish-json
                          {:path (-> route :route-params :path)
                           :id id}]))
    (re-frame/dispatch [::events/set-active-panel panel])))

(defonce history
  (pushy/pushy dispatch parse))

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
