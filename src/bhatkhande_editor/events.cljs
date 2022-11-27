(ns bhatkhande-editor.events
  (:require
   [re-frame.core :as re-frame
    :refer [debug reg-event-db reg-event-fx
            subscribe dispatch dispatch-sync]]
   [bhatkhande-editor.db :as db]
   ["firebase/app" :default firebase]
   ["firebase/auth" :default fbauth]
   ["firebase/storage" :default storage]
   ["firebase/firestore" :as firebase-firestore]
   [cognitect.transit :as t]
   ))

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
    (println " navigaate " handler)
   {:navigate handler}))

(re-frame/reg-event-fx
 ::set-active-panel
 (fn [{:keys [db]} [_ active-panel]]
   (println " setting active panel "active-panel)
   {:db (assoc db :active-panel active-panel)}))

(reg-event-fx
 ::conj-svara
 (fn [{:keys [db]} [_ {:keys [svara notes-per-beat]}]]
   (let [cpos (get-in db [:edit-props :cursor-pos ] )
         next-index(get-in db [:composition :index-forward-seq (vals cpos)])
         prev-index (get-in db [:composition :index-backward-seq (vals cpos)])
         _ (println " cur pos" cpos
                    " prev index " prev-index
                    " next index " next-index)

         note-index
         (if (nil? prev-index)
           -1
           (db/get-noteseq-index
            (zipmap [:row-index :bhaag-index :note-index :nsi] prev-index)
            (get-in db [:composition :taal])))
         ;;_ (println " pie2 " note-index " count of ns " (count (get-in db [:composition :noteseq])))
         ln (get-in db [:composition :noteseq note-index])
         nsvara (assoc svara :npb notes-per-beat)
         ;;_ (println nsvara " note at index " ln " notes per beat " notes-per-beat)
         noteseq-up-fn
         #(let [note-insert
                (if (= -1 note-index)
                  ;;insert note before rest of seq cos this is at 0 position
                  (into [{:notes [nsvara]}] %)
                  (into (conj (subvec % 0 (inc note-index)) {:notes [nsvara]})
                        (subvec % (inc note-index))))
                next-note-cursor
                (zipmap [:row-index :bhaag-index :note-index :nsi] next-index)
                res
                (if (> notes-per-beat 1)
                  ;;2 states here: either adding the first note of a multi-note
                  (do #_(println " a1 "
                               (and (not= notes-per-beat (-> ln :notes count))
                                    (= notes-per-beat (-> ln :notes last :npb))))
                      ;;if not the same as the last npb
                      (if
                          (and (not= notes-per-beat (-> ln :notes count))
                           (= notes-per-beat (-> ln :notes last :npb)))
                        [(update-in % [note-index :notes] conj nsvara)
                         (do #_(println " a2 "(> notes-per-beat (-> ln :notes count)))
                             (if (> notes-per-beat (-> ln :notes count))
                               ;;if multi-note will be full after this, stay on the same note
                               ;;don't increment the cursor
                               :cur-cursor
                               ;;increment just note-sub-index
                               (zipmap [:row-index :bhaag-index :note-index :nsi]
                                       (mapv (update-in cpos [:nsi] inc)
                                             [:row-index :bhaag-index :note-index :nsi]))))]
                        [note-insert :next-note-cursor]
                        ;;otherwise append to multi-note
                        ))
                  [note-insert :next-note-cursor])]
            res)
         [updated-ns updated-cursor] (noteseq-up-fn (get-in db [:composition :noteseq]))
         ndb
         (-> db 
             (update-in [:composition :noteseq] (constantly updated-ns))
             (update-in [:composition] db/add-indexes))
         ndb
         (-> ndb
             (update-in [:edit-props :cursor-pos]
                        (constantly
                         (cond
                           (= updated-cursor :next-note-cursor)
                           (let [next-index
                                 (get-in ndb [:composition :index-forward-seq (vals cpos)])
                                 prev-index
                                 (get-in ndb [:composition :index-backward-seq (vals cpos)])
                                 k1 (zipmap [:row-index :bhaag-index :note-index :nsi] next-index)
                                 ]
                             #_(println " 00 cur pos" cpos
                                      " prev index " prev-index
                                      " next index " next-index
                                      " k1 " k1)
                             k1)
                           (= updated-cursor :cur-cursor) cpos
                           :else
                           updated-cursor))))]
     {:db ndb})))

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
                              (zipmap [:row-index :bhaag-index :note-index :nsi]
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

(defn to-trans [x]
  (let [w (t/writer :json)]
    (t/write w x)))

;; media and cloud
(reg-event-fx
 ::upload-comp-json
 (fn [{:keys [db]} [_ file-name]]
   (let [comp (to-trans (select-keys (-> db :composition) [:noteseq :taal]))
         uuid (last (.split (.toString (random-uuid)) #"-"))
         _ (println uuid " last notes "
                    (->> db :composition :noteseq (drop 20)))
         path (str (-> db :user :uid) "/" uuid "-" file-name )
         stor (.storage firebase)
         storageRef (.ref stor)
         file-ref  (.child storageRef path)]
     (-> (.putString file-ref comp)
         (.then
          (fn[i]
            ;;handle failure of upload
            (.then
             (.getDownloadURL file-ref)
             (fn[iurl]
               (dispatch [::submission-completed? true])
               (dispatch [::get-short-url path])
               (dispatch [::update-bandish-url path]))))))
     {})))

(reg-event-fx
 ::submission-completed?
 (fn [{:keys [db]} [_ _]]
   {:db db}))

(reg-event-fx
 ::get-short-url
 (fn [{:keys [db]} [_ path]]
   (let [tr (t/reader :json)]
     (-> (js/fetch
          (str "https://firebasedynamiclinks.googleapis.com/v1/shortLinks?key=" db/apiKey)
          #js {"method" "post"
               "body" (.stringify js/JSON
                                  #js {"longDynamicLink"
                                       (db/get-long-url path)})})
         (.then (fn[i] (.json i)))
         (.then (fn[i]
                  (let []
                    (println " json resp " i))))
         (.catch (fn[i] (println " error " i ))))
     {:db db})))

(reg-event-fx
 ::update-bandish-url
 (fn [{:keys [db]} [_ bandish-json-url]]
   {:db
    (-> db
        (update-in [:bandish-url]
                   (constantly bandish-json-url)))}))

(reg-event-fx
 ::sign-in
 (fn [{:keys [db]} _]
   (let [storage (.-sessionStorage js/window)]
     ;;set a local storage because
     ;;when the auth redirect is sent back to the page,
     ;;the local DB atom will not remember and will load its
     ;;original clean slate.
     (.setItem storage "sign-in" "inprogress")
     {:db (-> db
              (dissoc :firebase-error))
      :firebase/google-sign-in {:sign-in-method :redirect}})))

(reg-event-fx
 ::sign-out
 (fn [{:keys [db]} _]
   {:db (dissoc db :user)
    :firebase/sign-out nil}))

(reg-event-fx
 ::set-user
 (fn [{:keys [db]}[_ user]]
   (if (and user (:email user))
     (let [storage (.-sessionStorage js/window)]
       (when storage
         (do ;;(println " removeItem called")
           (.removeItem storage "sign-in")))
       ;;set the id token
       #_(.then (.getIdToken (:user user))
                #(dispatch [::set-firebase-idtoken %]))
       {:db (-> db (assoc :user user)
                (dissoc :user-nil-times))
        })
     ;;the first event is user nil and the second one has the user mapv
     ;;therefor if it is set nil twice, then show login popup
     {:db (update-in db [:user-nil-times] (fnil inc 1))
      ;;:dispatch [::set-active-panel :login-panel]
      })))

(reg-event-fx
 ::firebase-error
 (fn [{:keys [db]} _]
   (println " fb auth error ")
   {:db db}))

(reg-event-fx
 ::set-bandish-id
 (fn [{:keys [db]} [_ id]]
   {:db (assoc db :bandish-id id)}))

(reg-event-fx
 ::get-bandish-json
 (fn [{:keys [db]} [_ {:keys [path id]}]]
   (let [tr (t/reader :json)]
     (-> (js/fetch
          (db/get-bandish-url (str path "/" id))
          #js {"method" "get"})
         (.then (fn[i] (.text i)))
         (.then (fn[i]
                  (let [imap (js->clj (t/read tr i))
                        res (db/comp-decorator imap)]
                    (println " noteseqq " (keys imap) " - "
                             (->> res :composition :noteseq (drop 20)))
                    (dispatch [::refresh-comp res]))))
         (.catch (fn[i] (println " error " i ))))
     {:db db})))


(reg-event-fx
 ::refresh-comp
 (fn [{:keys [db]} [_ {:keys [composition edit-props]}]]
   {:db (-> db
            (update-in [:composition] (constantly composition))
            (update-in [:edit-props] (constantly edit-props)))
    :dispatch [::navigate :home]}))
