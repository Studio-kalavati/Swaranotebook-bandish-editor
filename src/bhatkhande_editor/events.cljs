(ns bhatkhande-editor.events
  (:require
   [re-frame.core :as re-frame
    :refer [debug reg-event-db reg-event-fx
            subscribe dispatch dispatch-sync]]
   [bhatkhande-editor.db :as db]))

(defn get-ns-index
  [db]
  (let [{:keys [row-index bhaag-index note-index ni] :as click-index}
        (get-in db [:edit-props :cursor-pos])
        nindex (db/get-noteseq-index click-index (get-in db [:composition :taal]))]
    [nindex ni]))

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
 ::conj-svara
 (fn [{:keys [db]} [_ {:keys [svara notes-per-beat]}]]
   (let [cpos (get-in db [:edit-props :cursor-pos ] )
         index-entry (get-in db [:composition :index-forward-seq (vals cpos)])
         prev-index (get-in db [:composition :index-backward-seq (vals cpos)])
         _ (println " cur pos" cpos
                    " prev index " prev-index
                    " next index " index-entry)
         note-index (db/get-noteseq-index
                     (zipmap [:row-index :bhaag-index :note-index :ni]
                             prev-index)
                     (get-in db [:composition :taal]))
         _ (println " pie2 " note-index)
         ln (get-in db [:composition :noteseq note-index])
         nsvara (assoc svara :npb notes-per-beat)
         _ (println nsvara " note at index " ln 
                    " notes per beat " notes-per-beat)
         noteseq-up-fn
         #(let [note-insert
                (into (conj (subvec % 0 (inc note-index)) {:notes [nsvara]})
                      (subvec % (inc note-index)))
                next-note-cursor
                (zipmap [:row-index :bhaag-index :note-index :ni] index-entry)
                res
                (if (> notes-per-beat 1)
                  ;;2 states here: either adding the first note of a multi-note
                  (do (println " a1 "
                               (and (not= notes-per-beat (-> ln :notes count))
                                    (= notes-per-beat (-> ln :notes last :npb))))
                      ;;if not the same as the last npb
                      (if
                          (and (not= notes-per-beat (-> ln :notes count))
                           (= notes-per-beat (-> ln :notes last :npb)))
                        [(update-in % [note-index :notes] conj nsvara)
                         (do (println " a2 "(= notes-per-beat (-> ln :notes count inc)))
                             (if (> notes-per-beat (-> ln :notes count))
                               ;;if multi-note will be full after this, jump to next full note
                               cpos
                               (zipmap [:row-index :bhaag-index :note-index :ni]
                                       (mapv (update-in cpos [:ni] inc)
                                             [:row-index :bhaag-index :note-index :ni]))))]
                        [note-insert next-note-cursor]
                        ;;otherwise append to multi-note
                        ))
                  [note-insert next-note-cursor])]
            res)
         [updated-ns updated-cursor] (noteseq-up-fn (get-in db [:composition :noteseq]))
         ndb
         (-> db 
             (update-in [:composition :noteseq] (constantly updated-ns))
             (update-in [:edit-props :cursor-pos] (constantly updated-cursor)))]
     {:db ndb
      :dispatch [::index-noteseq]})))

(reg-event-fx
 ::conj-sahitya
 (fn [{:keys [db]} [_ {:keys [text-val bhaag-index row-index]}]]
   (let [indx (db/get-noteseq-index {:row-index row-index
                                     :bhaag-index bhaag-index
                                     :note-index 0}
                                    (get-in db [:composition :taal]))]
     {:db (-> db
              (update-in
               [:composition :noteseq indx :lyrics]
               (constantly text-val)))})))

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

(reg-event-fx
 ::show-text-popup
 (fn [{:keys [db]} [_ {:keys [row-index bhaag-index text-val] :as imap}]]
   {:db
    (-> db
        (update-in [:edit-props :show-text-popup]
                   (constantly imap)))}))

(reg-event-fx
 ::hide-text-popup
 (fn [{:keys [db]} [_ _]]
   {:db
    (-> db
        (update-in [:edit-props :show-text-popup]
                   (constantly false)))}))

(reg-event-fx
 ::delete-single-swara
 (fn [{:keys [db]} [_ _]]
   (let [[note-index note-sub-index] (get-ns-index db)
         cpos (get-in db [:edit-props :cursor-pos ] )
         index-entry (get-in db [:composition :index-backward-seq (vals cpos)])]
     (println " delete index "[note-index note-sub-index]
              " cpos " cpos
              " entry at index " index-entry)
     ;;need to update cursor position too
     {:db
      (-> db
          (update-in [:composition :noteseq]
                     #(let [to-remove (% note-index)
                            _ (println " to-remove " to-remove )
                            res
                            (if (= 0 note-index)
                              ;;dont delete  the first one
                              %
                              (into (subvec % 0 (dec note-index)) (subvec % note-index)))]
                        res))
          (update-in [:edit-props :cursor-pos]
                     (constantly
                      (let [res 
                            (if (= 0 note-index)
                              cpos
                              (zipmap [:row-index :bhaag-index :note-index :ni]
                                      (if (> (last index-entry) 0 )
                                        ;;if deleting a multi-note, the ni is > 0
                                        ;;instead make it 0
                                        (conj (subvec index-entry 0 3) 0)
                                        index-entry)))]
                        res))))
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
