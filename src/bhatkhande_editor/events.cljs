(ns bhatkhande-editor.events
  (:require
   [re-frame.core :as re-frame
    :refer [debug reg-event-db reg-event-fx
            subscribe dispatch dispatch-sync]]
   [bhatkhande-editor.db :as db]))

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
 (fn [{:keys [db]} [_ {:keys [swara notes-per-beat]}]]
   (let [ln (last (get-in db [:composition :m-noteseq]))
         ndb
         (if (:part (first ln))
           (if (= notes-per-beat (count ln))
             ;;add a new note
             (update-in db [:composition :m-noteseq] conj [(assoc swara :part true)])
             (let [mnoteseq (get-in db [:composition :m-noteseq])
                   irest (-> mnoteseq butlast vec)]
               ;;replace the last note and conj the new note
               (update-in db [:composition :m-noteseq]
                          (constantly (conj irest (conj ln swara))))))
           (update-in db [:composition :m-noteseq] conj [(assoc swara :part true)]))]
     {:db ndb})))

(reg-event-fx
 ::conj-single-swara
 (fn [{:keys [db]} [_ svara]]
   {:db (update-in db [:composition :m-noteseq] conj svara)}))

(reg-event-fx
 ::delete-single-swara
 (fn [{:keys [db]} [_ _]]
   {:db (update-in db [:composition :m-noteseq] #(-> % butlast vec))}))

(reg-event-fx
 ::set-raga
 (fn [{:keys [db]} [_ raga]]
   {:db (update-in db [:edit-props :raga] (constantly raga))}))

