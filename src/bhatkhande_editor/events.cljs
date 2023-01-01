(ns bhatkhande-editor.events
  (:require
   [re-frame.core :as re-frame
    :refer [debug reg-event-db reg-event-fx
            subscribe dispatch dispatch-sync]]
   [chronoid.core :as c]
   [bhatkhande-editor.db :as db]
   ["firebase/app" :default firebase]
   ["firebase/auth" :default fbauth]
   ["firebase/storage" :default storage]
   ["firebase/firestore" :as firebase-firestore]
   [cognitect.transit :as t]
   [sargam.talas :refer [taal-def]]
   ))

(defn get-ns-index
  [db]
  (let [{:keys [row-index bhaag-index note-index ni] :as click-index}
        (get-in db [:edit-props :cursor-pos])
        nindex (db/get-noteseq-index click-index (get-in db [:composition :taal]))]
    [nindex ni]))

(defn sched-play-url
  ([ctx start-at dur absn ] (sched-play-url ctx start-at dur absn {}))
  ([ctx start-at dur absn options]
   (let [ctime (.-currentTime ctx)
         {:keys [gain] :or {gain 1}} options
         gain-node (.createGain ctx)]
     (set! (.-value (.-gain gain-node)) gain)
     (-> (.connect absn gain-node)
         (.connect (.-destination ctx)))

     (if dur
       (.start absn (+ ctime start-at) 0 dur)
       (.start absn 0)))))

(defn play-url
  ([ctx absn] (play-url 0 nil ctx absn))
  ([start-at dur ctx absn ]
   (let [ctime (.-currentTime ctx)]
     (.connect absn (.-destination ctx))
     (if dur
       (.start absn (+ ctime start-at) 0 dur)
       (.start absn 0)))))

(reg-event-db
 ::initialize-db
 (fn [_ _]
   (let [storage (.-sessionStorage js/window)
         comp-str (.getItem storage "comp")
         ]
   (if comp-str
     (let [w (t/reader :json)
           comp (t/read w comp-str)
           {:keys [composition edit-props]} (db/comp-decorator comp)]
       (-> db/default-db
           (update-in [:composition] (constantly composition))
           (update-in [:edit-props] (constantly edit-props))))
     db/default-db))))

(reg-event-fx
  ::navigate
  (fn [_ [_ handler]]
   {:navigate handler}))

(reg-event-fx
 ::set-active-panel
 (fn [{:keys [db]} [_ active-panel]]
   {:db (assoc db :active-panel active-panel)
    :dispatch [::post-log {:payload (str "actice panel is " active-panel)}]}))

(defn play-shruti
  [db [shruti start-at dur options]]
  (let [audctx (:audio-context db)
        buf (@(:sample-buffers db) shruti)
        absn (new js/AudioBufferSourceNode audctx #js {"buffer" buf})]
    (sched-play-url audctx start-at dur absn options)
    absn))

(reg-event-fx
 ::play-svara
 (fn [{:keys [db]} [_ shruti]]
   (let [audctx (:audio-context db)
         buf (@(:sample-buffers db) shruti)
         absn (new js/AudioBufferSourceNode audctx
                   #js {"buffer" buf})]
     (if audctx
       (do (play-url audctx absn)
           {})
       (let [audctx (js/AudioContext.)]
         (play-url audctx absn)
         {:db (assoc db :audio-context audctx)})))))

(reg-event-fx
 ::conj-svara
 (fn [{:keys [db]} [_ {:keys [svara notes-per-beat]}]]
   (let [cpos (get-in db [:edit-props :cursor-pos ] )
         next-index(get-in db [:composition :index-forward-seq (vals cpos)])
         prev-index (get-in db [:composition :index-backward-seq (vals cpos)])
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
                  (do
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
                                 k1 (zipmap [:row-index :bhaag-index :note-index :nsi] next-index)]
                             k1)
                           (= updated-cursor :cur-cursor) cpos
                           :else
                           updated-cursor))))]
     {:db ndb
      :dispatch [::save-to-localstorage]})))

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

(reg-event-fx
 ::save-to-localstorage
 (fn[{:keys [db]} [_ _]]
   (let [storage (.-sessionStorage js/window)
         w (t/writer :json)
         out-string
         (t/write w (select-keys (-> db :composition) [:noteseq :taal]))]
     (.setItem storage "comp" out-string)
     {})))

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
     #_(println " delete index "[note-index note-sub-index]
              " cpos " cpos
              " entry at index " index-entry)
     ;;need to update cursor position too
     {:db
      (-> db
          (update-in [:composition :noteseq]
                     #(let [to-remove (% note-index)
                            res
                            (if (= 0 note-index)
                              ;;dont delete  the first one
                              %
                              (into (subvec % 0 (dec note-index)) (subvec % note-index)))]
                        res))
          (update-in [:composition] db/add-indexes)
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
      :dispatch [::save-to-localstorage]})))

(reg-event-fx
 ::set-raga
 (fn [{:keys [db]} [_ raga]]
   {:db (update-in db [:edit-props :raga] (constantly raga))
    :dispatch [::save-to-localstorage]
    }))

(reg-event-fx
 ::set-taal
 (fn [{:keys [db]} [_ taal]]
   (let [ncomp (db/add-indexes (assoc (get-in db [:composition]) :taal taal))]
     {:db (update-in db [:composition] (constantly ncomp))
      :dispatch [::save-to-localstorage]})))

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
 (fn [{:keys [db]} [_ comp-title]]
   (let [comp-title (if comp-title comp-title (get-in db [:composition :title]))
         comp (to-trans (select-keys (-> db :composition) [:noteseq :taal]))
         uuid (last (.split (.toString (random-uuid)) #"-"))
         path (str (-> db :user :uid) "/" uuid "-" comp-title)
         stor (.storage firebase)
         storageRef (.ref stor)
         file-ref  (.child storageRef path)]
     (-> (.putString file-ref comp)
         (.then
          (fn[i]
            (dispatch [::update-bandish-url path])
            (.pushState (.-history js/window) #js {} ""
                        (db/get-long-url path))
            (dispatch [::set-active-panel :home-panel]))))
     {:dispatch [::set-active-panel :wait-for-save-completion]
      :db (update-in db [:composition :title] (constantly comp-title))})))

(reg-event-fx
 ::list-files
 (fn [{:keys [db]} [_ _]]
   (let [path (str  "/" (-> db :user :uid))
         stor (.storage firebase)
         storageRef (.ref stor path)]
     (-> (.listAll storageRef)
         (.then
          (fn[i]
            (let [fullpaths
                  (->> (map #(.-fullPath %) (.-items i)))]
              (dispatch [::my-bandishes fullpaths])))))
     {})))

(reg-event-fx
 ::my-bandishes
 (fn [{:keys [db]} [_ bandish-list]]
   {:db (assoc db :my-bandishes bandish-list)}))

(reg-event-fx
 ::update-bandish-url
 (fn [{:keys [db]} [_ bandish-url]]
   {:db
    (-> db
        (update-in [:bandish-url]
                   (constantly bandish-url)))}))

(reg-event-fx
 ::sign-in
 (fn [{:keys [db]} _]
   (let [storage (.-sessionStorage js/window)]
     ;;set a local storage because
     ;;when the auth redirect is sent back to the page,
     ;;the local DB atom will not remember and will load its
     ;;original clean slate.
     (.setItem storage "sign-in" "inprogress")
     (println " start sign in")
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
   (println " set user " user)
   (if (and user (:email user))
     (let [storage (.-sessionStorage js/window)]
       (when storage
         (do 
           (.removeItem storage "sign-in")))
       {:db (-> db
                (assoc :user user)
                (dissoc :user-nil-times))
        :dispatch [::list-files]})
     ;;the first event is user nil and the second one has the user mapv
     ;;therefor if it is set nil twice, then show login popup
     {:db (update-in db [:user-nil-times] (fnil inc 1))})))

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
                    (dispatch [::refresh-comp res]))))
         (.catch (fn[i] (println " error " i ))))
     {:db db})))

(reg-event-fx
 ::hide-keyboard
 (fn [{:keys [db]} [_ _]]
   {:db (update-in db [:edit-props :show-keyboard?]
                   (constantly false))}))

(reg-event-fx
 ::show-keyboard
 (fn [{:keys [db]} [_ ival]]
   {:db (update-in db [:edit-props :show-keyboard?]
                   (constantly ival))}))

(reg-event-fx
 ::refresh-comp
 (fn [{:keys [db]} [_ {:keys [composition edit-props]}]]
   {:db (-> db
            (update-in [:composition] (constantly composition))
            (update-in [:edit-props] (constantly edit-props)))
    :dispatch [::navigate :home]}))

(reg-event-fx
 ::init-audio-ctx
 (fn [{:keys [db]} [_ audctx]]
   {:db (if-not (:audio-context db)
          (assoc db :audio-context audctx)
          db)}))

(reg-event-fx
 ::show-lyrics?
 (fn [{:keys [db]} [_ ival]]
   {:db (assoc db :show-lyrics? ival)}))

(reg-event-fx
 ::newline-on-avartan?
 (fn [{:keys [db]} [_ ival]]
   {:db (assoc db :newline-on-avartan? ival)}))

(reg-event-fx
 ::set-bpm
 (fn [{:keys [db]} [_ ival]]
   {:db (assoc db :bpm ival)}))

(reg-event-fx
 ::metronome?
 (fn [{:keys [db]} [_ ival]]
   {:db (assoc db :metronome? ival)}))

(reg-event-fx
 ::tanpura?
 (fn [{:keys [db]} [_ ival]]
   {:db (assoc db :tanpura? ival)}))

(reg-event-fx
 ::play
 (fn [{:keys [db]} [_ _]]
   (let [note-interval (/ 60 (:bpm db) )
         {:keys [audio-context clock]} db
         now (.-currentTime audio-context)
         taal (-> db :composition :taal)
         metronome-on-at (set (->> taal-def taal
                                        :bhaags
                                        (reductions +) ;;[4 8 12 16]
                                        (map inc) ;;get start of next bhaag
                                        butlast ;;drop the last one and add the first note
                                        (into [1])))
         a1
         (->> db :composition :noteseq
              (map vector (range 0 (->> db :composition :noteseq count inc) note-interval)
                   (range))
              (map (fn[[at note-index {:keys [notes] :as ivec}]]
                     ;;make 0 based to 1 based index
                     (let [note-index (mod (inc note-index) (-> taal-def taal :num-beats))
                           notseq
                           (if (= 1 (count notes))
                             [[(-> notes first :shruti) (+ now at) note-interval]]
                             ;;if many notes in one beat, schedule them to play at equal intervals
                             (let [sub-note-intervals (/ note-interval (-> notes count))]
                               (mapv (fn[a b] [a (+ now b) sub-note-intervals])
                                     (map :shruti notes)
                                     (range at (+ at note-interval) sub-note-intervals))))]
                       (if (and (:metronome? db) (metronome-on-at note-index))
                         (into [[:tick2 (+ now at) note-interval]] notseq )
                         notseq))))
              (reduce into []))
         a1 (if (:tanpura? db)
              (let [last-note-time (- (-> a1 last second) now )
                    sample-len 3
                    ;;length of sample is 4 secs
                    play-n-times (int (/ last-note-time sample-len))
                    conj-vec (mapv 
                              #(vector :tanpura (+ now (* % sample-len)) sample-len {:gain 0.5})
                              (range (inc play-n-times)))]
                #_(println " lnt " (vector now (-> a1 last second)) " - "last-note-time " times " play-n-times " conj-vec " conj-vec)
                (vec (sort-by second
                              (into a1 conj-vec))))
              a1)
         ;;a1 contains notes, tanpura, beat sounds.
         ;;we need another index that translates a note index to the visual index which
         ;;contains just the notes
         a2 (->> a1
                 (map vector (range))
                 ;;select only notes encoded as [:mandra :s]
                 (filter (fn[[indx inote]] (vector? (first inote))))
                 (map vector (range))
                 (map (fn[[svara-index [nindex _]]] {nindex svara-index}))
                 (apply merge))]
     {:db (assoc db
                 :clock clock
                 :play-state :start
                 :play-at-time a1
                 :note-interval note-interval
                 ;;translates the play-note index to the view-note index
                 :play-to-view-map a2
                 :timer
                 (-> (c/set-timeout! clock #(dispatch [::clock-tick-event]) 0)
                     (c/repeat! 400)))
      :dispatch [::clock-tick-event]})))

(reg-event-fx
 ::pause
 (fn [{:keys [db]} [_ _]]
   (c/clear! (:timer db))
   {:db (-> (assoc db :play-state :pause)
            (dissoc :timer))}))

(reg-event-fx
 ::stop
 (fn [{:keys [db]} [_ _]]
   (c/clear! (:timer db))
   {:db (-> (assoc db :play-state :stop)
            (dissoc :timer)
            (assoc :play-note-index 0))}))

(reg-event-fx
 ::register-elem
 (fn [{:keys [db]} [_ index nsi elem]]
   ;;(println " register elem "[ index nsi elem])
   {:db (update-in db [:elem-index ] conj elem)}))

(reg-event-fx
 ::clock-tick-event
 (fn [{:keys [db]} [_ _]]
   (try
     (let [at (.-currentTime (:audio-context db))
           play-at-time (:play-at-time db)
           max-note-index (count play-at-time)
           play-note-index (or (:play-note-index db) 0)
           past-notes-fn (fn[time-index start-index time-change-fn]
                           (take-while
                            (fn[i]
                              (and (> max-note-index i)
                                   (let [[_ iat idur] (time-index i)]
                                     (>= at (time-change-fn iat)))))
                            (iterate inc start-index)))
           past-notes-to-play (past-notes-fn play-at-time play-note-index #(- % 0.5))
           idb {:db (if (-> past-notes-to-play empty? not)
                      (let [n-note-play-index
                            (if (-> past-notes-to-play empty? not)
                              (-> past-notes-to-play last inc)
                              play-note-index)]
                        (->> past-notes-to-play
                             (mapv (fn[ indx]
                                     (let [[inote iat idur :as noteat] (play-at-time indx)
                                           iat (- iat at)
                                           view-note-index ((:play-to-view-map db) indx)]
                                       (play-shruti db [inote (if (> iat 0) iat 0) idur
                                                        (if (= 4 (count noteat))
                                                          (last noteat) {})])
                                       (when view-note-index
                                         (let [notel (get-in (:elem-index db) [view-note-index])]
                                           (set! (.-style notel) (str "fill-opacity:0.2"))
                                           (->
                                            (.animate
                                             notel
                                             #js [
                                                  #js {"opacity" 0.5}
                                                  #js {"opacity" 1 "offset" 0.2}
                                                  #js {"opacity" 0}]
                                             #js {"duration"
                                                  (* 1000 2 (:note-interval db))})
                                            (.addEventListener
                                             "finish"
                                             (fn[e]
                                               (set! (.-style notel) "fill-opacity:0"))))))))))
                        (assoc db :play-note-index n-note-play-index))
                      db)}
           ret
           (if (= play-note-index (count play-at-time))
             (do
               (merge idb {:dispatch [::stop]}))
             idb)]
       ret)
     (catch js/Error e
       (println " caught error in clock-tick-event" e)
       {}))))

(reg-event-fx
 ::post-log
 (fn [{:keys [db]} [_ {:keys [payload ]}]]
   (let [body {"entries" [{"logName" (str "projects/" db/projectId "/logs/browserlog")
                           "resource" {"type" "global"
                                       "labels" {"projectId" db/projectId}}
                      "textPayload" payload} ]}]
     (-> (js/fetch (str "https://logging.googleapis.com/v2/entries:write?key=" db/apiKey)
          #js {"method" "post"
               "body" (.stringify js/JSON (clj->js body))})
         (.then (fn[i] (.text i)))
         (.then (fn[i]
                  (let []
                    (println " i "i))))
         (.catch (fn[i] (println " error " i ))))
     {:db db})))
