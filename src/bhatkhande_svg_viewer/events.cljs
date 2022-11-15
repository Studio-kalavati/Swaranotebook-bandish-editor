(ns bhatkhande-svg-viewer.events
  (:require
   [re-frame.core :as re-frame
    :refer [debug reg-event-db reg-event-fx
            subscribe dispatch dispatch-sync]]
   [bhatkhande-svg-viewer.db :as db]))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(re-frame/reg-event-fx
  ::navigate
  (fn [_ [_ handler]]
   {:navigate handler}))

(re-frame/reg-event-fx
 ::set-active-panel
 (fn [{:keys [db]} [_ active-panel]]
   {:db (assoc db :active-panel active-panel)}))

(reg-event-fx
 ::conj-part-swara
 (fn [{:keys [db]} [_ _]]
   {:db db}))

(reg-event-fx
 ::conj-single-swara
 (fn [{:keys [db]} [_ _]]
   {:db db}))

(reg-event-fx
 ::delete-single-swara
 (fn [{:keys [db]} [_ _]]
   {:db db}))
