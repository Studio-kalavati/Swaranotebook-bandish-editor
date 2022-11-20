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
   {:db (update-in db [:composition :m-noteseq] conj svara)
    :dispatch [::index-noteseq]}))

(reg-event-fx
 ::index-noteseq
 (fn [{:keys [db]} [_ _]]
   (let [indexed-ns (db/split-bhaags
                     (get-in db [:composition :m-noteseq])
                     (get-in db [:composition :taal]))]
     {:db (update-in db [:composition :indexed-noteseq]
                     (constantly indexed-ns))})))

(reg-event-fx
 ::delete-single-swara
 (fn [{:keys [db]} [_ _]]
   (let [{:keys [row-index bhaag-index note-index ni] :as click-index}
         (get-in db [:edit-props :cursor-pos])
         nindex (db/get-noteseq-index click-index (get-in db [:composition :taal]))]
     ;;need to update cursor position too
     {:db
      (update-in db
                 [:composition :m-noteseq]
                 #(let [to-remove (% nindex)
                        res
                        (if (= 1 (count to-remove))
                          ;;remove whole svara
                          (into (subvec % 0 nindex) (subvec % (inc nindex)))
                          (let [r2 (into (subvec to-remove 0 ni)
                                         (subvec to-remove (inc ni)))]
                            (update-in % [nindex] (constantly r2))))]
                        res))
      :dispatch [::index-noteseq]})))

(reg-event-fx
 ::set-raga
 (fn [{:keys [db]} [_ raga]]
   {:db (update-in db [:edit-props :raga] (constantly raga))}))

(reg-event-fx
 ::set-click-index
 (fn [{:keys [db]} [_ click-index]]
   {:db (update-in db [:edit-props :cursor-pos] (constantly click-index))}))

(reg-event-fx
 ::conj-note-index
 (fn [{:keys [db]} [_ click-index]]
   (let [ndb (update-in db [:edit-props :note-index ]
              conj click-index)]
     {:db ndb})))

(reg-event-fx
 ::reset-note-index
 (fn [{:keys [db]} [_ _]]
   (let [ndb (update-in db [:edit-props :note-index ] (constantly []))]
     {:db ndb})))
