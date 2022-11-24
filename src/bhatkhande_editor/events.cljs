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
   (let [
         [note-index note-sub-index] (get-ns-index db)
         cpos (get-in db [:edit-props :cursor-pos ] )
         index-entry (get-in db [:composition :index-forward-seq (vals cpos)])

         ln (get-in db [:composition :noteseq note-index])
         nsvara (assoc svara :npb notes-per-beat)
         _ (println nsvara " note at index " ln " cur pos" cpos  " next index entry " index-entry  " -- "[note-index note-sub-index])
         ndb
         (update-in db
          [:composition :noteseq]
          #(let [note-insert
                 (into (conj (subvec % 0 (inc note-index)) {:notes [nsvara]})
                       (subvec % (inc note-index)))
                 ;;if npb is 1, or the last note's npb is different, insert new note
                 res (if (or (-> ln :notes last :npb (not= notes-per-beat))
                             (= 1 notes-per-beat))
                       (do (println " a1 true "
                                    (vector (-> ln :notes last :npb (not= notes-per-beat))
                                            (= 1 notes-per-beat)))
                           note-insert)
                       (let [last-note-count (count ln)]
                         (println " a1 false, next "
                                  (= (count (:notes ln)) notes-per-beat))
                         ;;last note already have enough notes
                         (if (= (count (:notes ln)) notes-per-beat)
                           note-insert
                           (update-in % [note-index :notes] conj nsvara))))]
             res))
         ndb2
         (update-in ndb
                    [:edit-props :cursor-pos]
                    (constantly
                     (let [_ (println " ntest "
                                      (vector
                                       (-> ln :notes last :npb (= notes-per-beat))
                                       (not (= notes-per-beat (count (:notes ln))))))
                           res
                           (zipmap [:row-index :bhaag-index :note-index :ni]
                                   (if (and (-> ln :notes last :npb (= notes-per-beat))
                                            (not (= notes-per-beat (count (:notes ln)))))
                                     ;;increment the note-sub-index
                                     (mapv (update-in cpos [:ni] inc)
                                                  [:row-index :bhaag-index :note-index :ni])
                                     index-entry))]
                       (println " update cursor to " res)
                       res)))]
     {:db ndb2
      :dispatch [::index-noteseq]})))

(reg-event-fx
 ::conj-sahitya
 (fn [{:keys [db]} [_ {:keys [text-val bhaag-index row-index]}]]
   (let []
     (println " conj sahitya " [text-val bhaag-index row-index])
     {:db (-> db
              (update-in
               [:composition :sahitya [row-index bhaag-index]]
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
     ;;need to update cursor position too
     {:db
      (-> db
          (update-in [:composition :noteseq]
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
