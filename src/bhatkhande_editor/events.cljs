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



(defn get-last-noteseq-index
  "get the level 4 index from the indexed-noteseq"
  [indexed-ns]
  (let [[row-index bhaag-index note-index note-sub-index :as indx]
        [(count indexed-ns)
         (-> indexed-ns last count)
         (-> indexed-ns last last count)
         (-> indexed-ns last last last count)]
        res (mapv dec indx)]
    res))

(reg-event-fx
 ::index-noteseq
 (fn [{:keys [db]} [_ _]]
   (let [ncomp (db/add-indexes (get-in db [:composition]))]
     {:db
      (-> db
          (update-in [:composition]
                     (constantly ncomp)))})))

(defn get-ns-index
  [db]
  (let [{:keys [row-index bhaag-index note-index ni] :as click-index}
        (get-in db [:edit-props :cursor-pos])
        nindex (db/get-noteseq-index click-index (get-in db [:composition :taal]))]
    [nindex ni]))

(reg-event-fx
 ::delete-single-swara
 (fn [{:keys [db]} [_ _]]
   (let [[note-index note-sub-index] (get-ns-index db)
         cpos (get-in db [:edit-props :cursor-pos ] )
         index-entry (get-in db [:composition :index-backward-seq (vals cpos)])]
     ;;need to update cursor position too
     {:db
      (-> db
          (update-in [:composition :m-noteseq]
                     #(let [to-remove (% note-index)
                            res
                            (if (= 1 (count to-remove))
                              ;;remove whole svara
                              (into (subvec % 0 note-index) (subvec % (inc note-index)))
                              (let [r2 (into (subvec to-remove 0 note-sub-index)
                                             (subvec to-remove (inc note-sub-index)))]
                                (update-in % [note-index] (constantly r2))))]
                        res))
          (update-in [:edit-props :cursor-pos]
                     (constantly
                      (zipmap [:row-index :bhaag-index :note-index :ni] index-entry))))
      :dispatch [::index-noteseq]})))

(reg-event-fx
 ::conj-single-swara
 (fn [{:keys [db]} [_ svara]]
   (let [[note-index note-sub-index] (get-ns-index db)
         cpos (get-in db [:edit-props :cursor-pos ] )
         index-entry (get-in db [:composition :index-forward-seq (vals cpos)])
         ]
     (println " cur pos" cpos  " next index entry " index-entry  " -- "[note-index note-sub-index])
     {:db (-> db
              (update-in [:composition :m-noteseq]
                            #(into (conj (subvec % 0 (inc note-index)) svara )
                                   (subvec % (inc note-index))))
              (update-in [:edit-props :cursor-pos]
                         (constantly
                          (zipmap [:row-index :bhaag-index :note-index :ni] index-entry))))
      :dispatch [::index-noteseq]})))

(reg-event-fx
 ::set-raga
 (fn [{:keys [db]} [_ raga]]
   {:db (update-in db [:edit-props :raga] (constantly raga))}))

(reg-event-fx
 ::set-taal
 (fn [{:keys [db]} [_ taal]]
   (let [ncomp (db/add-indexes (assoc (get-in db [:composition]) :taal taal))]
     {:db (update-in db [:composition] (constantly ncomp))})))

(reg-event-fx
 ::toggle-lang
 (fn [{:keys [db]} [_ _]]
   {:db (update-in db [:edit-props :language-en?] not)}))

(reg-event-fx
 ::set-click-index
 (fn [{:keys [db]} [_ click-index]]
   {:db (update-in db [:edit-props :cursor-pos] (constantly click-index))}))

(reg-event-fx
 ::reset-note-index
 (fn [{:keys [db]} [_ _]]
   (let [ndb (update-in db [:edit-props :note-index ] (constantly []))]
     {:db ndb})))
