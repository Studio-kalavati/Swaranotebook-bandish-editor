(ns bhatkhande-editor.events
  (:require
   [sargam.spec :as us]
   [reagent.core :as reagent]
   [re-frame.core :as re-frame
    :refer [debug reg-event-db reg-event-fx
            subscribe dispatch dispatch-sync]]
   [chronoid.core :as c]
   [bhatkhande-editor.db :as db :refer [pitch-s-list]]
   ["firebase/app" :default firebase]
   ["firebase/auth" :default fbauth]
   ["firebase/storage" :default storage]
   ["firebase/firestore" :as firebase-firestore]
   ["posthog-js" :default posthog]
   [cognitect.transit :as t]
   [sargam.talas :refer [taal-def]]))

(def log-event
  (re-frame.core/->interceptor
   :id      :log-event
   :after  (fn [context]
             (let [[k v] (-> context :coeffects :event)]
               #_(println " event fired " [k v])
               (.capture (-> context :coeffects :db :posthog) (str k) (clj->js v) "")
                context))))

(defn get-ns-index
  [db]
  (let [{:keys [row-index bhaag-index note-index ni] :as click-index}
        (get-in db [:props :cursor-pos])
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
         ph (.init posthog db/posthogKey #js {"api_host" "https://app.posthog.com"
                                              "persistence" "memory"
                                              "enable_recording_console_log" true})]
     ;;dont read comp-str if not refresh.
     (-> (if comp-str
           (let [w (t/reader :json)
                 comp (t/read w comp-str)
                 {:keys [composition props]} (db/comp-decorator comp)]
             (-> db/default-db
                 (update-in [:composition] (constantly composition))
                 (update-in [:props] (constantly props))))
           db/default-db)
         (assoc :posthog ph)))))

(reg-event-fx
 ::navigate-to
 (fn [_ [_ path title]]
   {:navigate-to-comp [path title]}))

(reg-event-fx
  ::navigate
  (fn [_ [_ handler ]]
    {:navigate handler}))

(reg-event-fx
 ::set-active-panel
 [log-event]
 (fn [{:keys [db]} [_ active-panel]]
   {:db (assoc db :active-panel active-panel)}))

(defn play-sample
  [db [sample-key start-at dur options]]
  (let [audctx (:audio-context db)
        ;;santoor sounds are in note-buffers,
        ;;tabla, tanpura and ticks are in sample-buffers
        buffers (if (vector? sample-key) @(:note-buffers db)
                    @(:sample-buffers db))
        buf (buffers sample-key)
        absn (new js/AudioBufferSourceNode audctx #js {"buffer" buf})]
    (sched-play-url audctx start-at dur absn options)
    absn))

(reg-event-fx
 ::play-svara
 (fn [{:keys [db]} [_ shruti]]
   (if (:audio-context db)
     (let [audctx (:audio-context db)
           buf (@(:note-buffers db) shruti)
           absn (new js/AudioBufferSourceNode audctx
                     #js {"buffer" buf})]
       (if audctx
         (do (play-url audctx absn)
             {})
         (let [audctx (js/AudioContext.)]
           (play-url audctx absn)
           {:db (assoc db :audio-context audctx)})))
     {:dispatch [::init-note-buffers]})))

(reg-event-fx
 ::conj-svara
 [log-event]
 (fn [{:keys [db]} [_ {:keys [svara notes-per-beat]}]]
   (let [cpos (get-in db [:props :cursor-pos ] )
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
             (update-in [:props :cursor-pos]
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
 [log-event]
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
        (update-in [:props :show-text-popup]
                   (constantly imap)))}))

(reg-event-fx
 ::hide-text-popup
 (fn [{:keys [db]} [_ _]]
   {:db
    (-> db
        (update-in [:props :show-text-popup]
                   (constantly false)))}))

(reg-event-fx
 ::delete-single-swara
 [log-event]
 (fn [{:keys [db]} [_ _]]
   (let [[note-index note-sub-index] (get-ns-index db)
         cpos (get-in db [:props :cursor-pos ] )
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
          (update-in [:props :cursor-pos]
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
 ::set-custom-svaras
 [log-event]
 (fn [{:keys [db]} [_ svaras]]
   {:db
    (-> (update-in db [:props :raga] (constantly :custom))
        (update-in [:props :custom-svaras] (constantly svaras)))}))

(reg-event-fx
 ::set-raga
 [log-event]
 (fn [{:keys [db]} [_ raga]]
   {:db (update-in db [:props :raga] (constantly raga))
    ;;:dispatch [::save-to-localstorage]
    }))

(reg-event-fx
 ::set-taal
 [log-event]
 (fn [{:keys [db]} [_ taal]]
   (let [ncomp (db/add-indexes (assoc (get-in db [:composition]) :taal taal))]
     {:db (update-in db [:composition] (constantly ncomp))
      :dispatch [::save-to-localstorage]})))

(reg-event-fx
 ::toggle-lang
 [log-event]
 (fn [{:keys [db]} [_ _]]
   {:db (update-in db [:props :lang]
                   (fn[i]
                     (cond
                       (= i :english) :hindi
                       (= i :hindi) :bangla
                       :else :english)))}))

(reg-event-fx
 ::set-click-index
 [log-event]
 (fn [{:keys [db]} [_ click-index]]
   {:db (update-in db [:props :cursor-pos] (constantly click-index))}))

(reg-event-fx
 ::reset-note-index
 (fn [{:keys [db]} [_ _]]
   (let [ndb
         (-> (update-in db [:props :note-index ] (constantly []))
             (update-in [:elem-index ] (constantly [])))]
     {:db ndb})))

(defn to-trans [x]
  (let [w (t/writer :json)]
    (t/write w x)))

;; media and cloud
#_(reg-event-fx
 ::update-bandish-url
 (fn [{:keys [db]} [_ bandish-url]]
   {:db
    (-> db
        (update-in [:bandish-url]
                   (constantly bandish-url)))}))

(defn upload-comp
  [uid title bandish]
  (let [stor (.storage firebase)
        storageRef (.ref stor)
        path (str uid "/" title)
        file-ref  (.child storageRef path)]
    (-> (.putString file-ref bandish)
        (.then
         (fn[i]
           (dispatch [::navigate-to uid title])
           #_(.pushState (.-history js/window) #js {} ""
                       (db/get-long-url path))
           (dispatch [::set-active-panel :home-panel]))))))

(reg-event-fx
 ::upload-new-comp
 [log-event]
 (fn [{:keys [db]} [_ comp-title]]
   (let [ndb (if comp-title
               (update-in db [:composition :title] (constantly comp-title))
               db)
         comp (->
               (select-keys (-> ndb :composition) [:noteseq :taal :title])
               to-trans)
         path (str (last (.split (.toString (random-uuid)) #"-"))
                   "-" comp-title)]
     (upload-comp (-> ndb :user :uid) path comp)
     {:dispatch [::set-active-panel :wait-for-save-completion]
      :db ndb})))

(reg-event-fx
 ::delete-comp
 [log-event]
 (fn [{:keys [db]} [_ ipath]]
   (let [stor (.storage firebase)
         storageRef (.ref stor ipath)]
     (-> (.delete storageRef)
         (.then (fn[i] (println ipath " delete " i " success" )
                  (dispatch [::list-files])))
         (.catch (fn[i] (println ipath " delete " i " failed " ))))
     {:db db
      :dispatch [::set-active-panel :wait-for-loading-comps]})))

(reg-event-fx
 ::upsert-comp
 [log-event]
 (fn [{:keys [db]} [_ _]]
   (let [comp (->
               (select-keys (-> db :composition) [:noteseq :taal])
               to-trans)
         comp-title (get-in db [:composition :title])
         path
         (if-let [p2 (-> db :props :id)]
             ;;if the current path is a pre-saved comp, use that and overwrite
           p2
           (str (last (.split (.toString (random-uuid)) #"-")) "-" comp-title))]
     (upload-comp (-> db :user :uid) path comp)
     {:dispatch [::set-active-panel :wait-for-save-completion]
      :db db})))

(reg-event-fx
 ::list-files
 [log-event]
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
     {:dispatch [::set-active-panel :wait-for-loading-comps]})))

(reg-event-fx
 ::my-bandishes
 [log-event]
 (fn [{:keys [db]} [_ bandish-list]]
   {:db (assoc db :my-bandishes bandish-list)
    :dispatch [::set-active-panel :list-comps-panel]}))

(reg-event-fx
 ::google-sign-in-fx
 [log-event]
 (fn [{:keys [db]} [_ _]]
   (let [storage (.-sessionStorage js/window)]
     ;;set a local storage because
     ;;when the auth redirect is sent back to the page,
     ;;the local DB atom will not remember and will load its
     ;;original clean slate.
     ;;(.setItem storage "sign-in" "inprogress")
     {:db (-> db
              (dissoc :firebase-error))
      :firebase/google-sign-in {:sign-in-method :redirect}})))

(reg-event-fx
 ::get-group-info
 [log-event]
 (fn [{:keys [db]} [_ {:keys [email display-name]}]]
   (let []
     (-> (js/fetch (str "https://connect.mailerlite.com/api/groups" )
                   #js {"method" "get"
                        "headers"
                        #js {"Authorization" (str "Bearer " db/mailerliteApiToken)
                             "Content-Type" "application/json"
                             "Accept" "application/json"}})
         (.then (fn[i] (.text i)))
         (.then (fn[i]
                  (let []
                    (println " i "i))))
         (.catch (fn[i] (println " error " i ))))
     {:db db})))

(reg-event-fx
 ::add-contact-to-newsletter
 [log-event]
 (fn [{:keys [db]} [_ {:keys [email display-name]}]]
   (let [body {"email" email
               "fields" {"display-name" display-name}
               ;;signups group id
               "groups" [db/mailerliteGroupId]}]
     (-> (js/fetch (str "https://connect.mailerlite.com/api/subscribers" )
                   #js {"method" "post"
                        "headers"
                        #js {"Authorization" (str "Bearer " db/mailerliteApiToken)
                             "Content-Type" "application/json"
                             "Accept" "application/json"}
                        "body" (.stringify js/JSON (clj->js body))})
         (.then (fn[i] (.text i)))
         (.then (fn[i]
                  (let []
                    (println " added to email "))))
         (.catch (fn[i] (println " error " i ))))
     {:db db})))

(reg-event-fx
 ::sign-out
 [log-event]
 (fn [{:keys [db]} _]
   {:db (dissoc db :user)
    :firebase/sign-out nil}))

(reg-event-fx
 ::set-user
 [log-event]
 (fn [{:keys [db]}[_ user]]
   (try 
     (if (and user (:email user))
       (let [storage (.-sessionStorage js/window)
             newsletter-signup?
             (if (and storage
                      (= "true"
                         (.getItem storage "newsletter-subscribe?")))
               (do
                 (.removeItem storage "newsletter-subscribe?")
                 true)
               false)
             ndb {:db (-> db
                          (assoc :user user)
                          (dissoc :user-nil-times))
                  ;;:dispatch [::get-group-info]
                  }]
         (when storage
           (do
             (.removeItem storage "sign-in")))
         (if newsletter-signup?
           (do
             (assoc ndb :dispatch [::add-contact-to-newsletter
                                   (select-keys user [:email :display-name])]))
           ndb))
       ;;the first event is user nil and the second one has the user mapv
       ;;therefor if it is set nil twice, then show login popup
       (do 
         #_(println " user not set ")
       {:db (update-in db [:user-nil-times] (fnil inc 1))}))
     (catch js/Error e
       (println " got error in set-user " e)
       {}))))

(reg-event-fx
 ::firebase-error
 [log-event]
 (fn [{:keys [db]} [_ k]]
   (println " fb auth error " k)
   {:db db}))

(reg-event-fx
 ::set-bandish-id
 (fn [{:keys [db]} [_ id]]
   {:db (assoc db :bandish-id id)}))

(reg-event-fx
 ::set-query-params
 (fn [{:keys [db]} [_ qp]]
   (let [qp1 (if (:beat-mode qp)
               (update-in qp [:beat-mode] keyword)
               qp)
         nep (-> (update-in db [:props] merge qp1)
                 (update-in [:props :mode] keyword))]
     {:db nep})))

(reg-event-fx
 ::set-url-path
 (fn [{:keys [db]} [_ {:keys [path id]}]]
   {:db (update-in db [:props] assoc :path path :id id)}))

(reg-event-fx
 ::clear-url-path
 (fn [{:keys [db]} [_ _]]
   {:db (update-in db [:props] dissoc :path :id)}))

(reg-event-fx
 ::get-bandish-json
 [log-event]
 (fn [{:keys [db]} [_ {:keys [path id] :as urlparams}]]
   (let [tr (t/reader :json)]
     (-> (js/fetch
          (db/get-bandish-url (str path "/" id))
          #js {"method" "get"})
         (.then (fn[i] (.text i)))
         (.then (fn[i]
                  (let [imap (js->clj (t/read tr i))]
                    (dispatch [::set-url-path urlparams])
                    (dispatch [::refresh-comp imap]))))
         (.catch (fn[i] (println " error " i ))))
     {:db db})))

(defn fetch-url
  ([max-elements imap ctx ikey iurl]
   (->
    (js/fetch iurl #js {"mode" "cors"})
    (.then (fn [r] (.arrayBuffer r)))
    (.then (fn [r] (.decodeAudioData ctx r)))
    (.then (fn [resp]
             (when (>= (count (keys @imap)) max-elements)
               (dispatch [::set-active-panel :home-panel]))
             (swap! imap assoc ikey resp))))))

#_(defn get-metronome-sample-loc
  [imap ctx]
    (mapv (partial fetch-url 2 imap ctx)
          [:tick1 :tick2]
          (map #(str "/sounds/metronome/metro" % ".mp3") [1 2]))
    imap)

(defn get-tabla-sample-loc
  [imap ctx]
  (let [taal-list ["ektaal" "dadra" "rupak" "teentaal" "jhaptaal" "kehrwa" "adachautaal"]
        beat-intervals (range 60 310 15)
        ;;3 added for ticks and tanpura
        ag-note-seq pitch-s-list
        sample-count (dec (+ 2 (count ag-note-seq) (* (count taal-list) (count beat-intervals))))
        _ (mapv (partial fetch-url sample-count imap ctx)
                (map keyword ag-note-seq)
                (map #(str "/sounds/tanpura/" % ".mp3") ag-note-seq ))
        _ (mapv (partial fetch-url sample-count imap ctx)
                [:tick1 :tick2]
                (map #(str "/sounds/metronome/metro" % ".mp3") [1 2]))
        ;;tabla samples
        ifn (fn[taal]
              (let [paths (map #(str "/sounds/tabla/" taal "/" taal % "bpm.mp3") beat-intervals)
                    kws (map #(keyword (str taal % "bpm")) beat-intervals)]
                (mapv (partial fetch-url sample-count imap ctx) kws paths)))]
    (count (mapv ifn taal-list)))
  imap)

(def svara-keys
  (conj (vec (for [i [:mandra :madhyam :taar] j (take 12 us/i-note-seq)]
               [i j])) [:ati-taar :s]))

(defn get-santoor-url-map
  ([imap ctx ] (get-santoor-url-map 0 imap ctx))
  ([pitches-above-c imap ctx ]
   (let [ivals (mapv
                #(str "/sounds/santoor/" %)
                (-> (for [i ["4" "5" "6"]
                          j pitch-s-list]
                      (str  j i ".mp3"))
                    vec
                    (conj "c7.mp3")))
         [skeys samples]
         (if (nil? pitches-above-c)
           [svara-keys ivals]
           (let [{:keys [id]} pitches-above-c]
             (if (= id 0)
               [svara-keys ivals]
               (if (< id 7)
                 [svara-keys (drop id ivals)]
                 [(drop id svara-keys) ivals]))))
         ;;return the lower of the two
         count-samples (dec (let [[s1 s2] (mapv count [skeys samples])]
                              (if (> s1 s2) s2 s1)))
         ibuffers (mapv (partial fetch-url count-samples imap ctx) skeys samples)]
     imap)))

(defn get-clock
  []
  (let  [clock (c/clock)
         _ (c/start! clock)
         ctx (:context @clock)]
    {:clock clock :audio-context ctx}))

(reg-event-fx
 ::init-audio-buffers
 (fn [{:keys [db]} [_ _]]
   (let  [{:keys [clock audio-context] :as clk-ctx} (get-clock)
          bufatom (get-tabla-sample-loc (reagent/atom {}) audio-context)]
     {:db (merge (assoc db :sample-buffers bufatom) clk-ctx)
      :dispatch [::set-active-panel :load-sounds-panel]})))

(reg-event-fx
 ::init-note-buffers
 (fn [{:keys [db]} [_ pitches-above-c]]
   (let [{:keys [clock audio-context] :as clk-ctx} (get-clock)
         bufatom (reagent/atom {})
         bufatom (get-santoor-url-map pitches-above-c bufatom audio-context )
         new-pitch (or (:sample pitches-above-c) "c")]
     {:db (-> (merge (assoc db :note-buffers bufatom) clk-ctx )
              (update-in [:props :pitch] (constantly new-pitch)))
      :dispatch [::set-active-panel :load-sounds-panel]})))

(reg-event-fx
 ::set-mode
 [log-event]
 (fn [{:keys [db]} [_ ival]]
   (let [ndb (update-in db [:props :mode]
                        (constantly ival))]
     (if (= :play ival)
         (cond (nil? (:audio-context ndb))
           (assoc {:db ndb} :dispatch-n [[::init-audio-buffers] [::init-note-buffers]])
           (nil? (:sample-buffers ndb))
           {:db ndb :dispatch [::init-audio-buffers]}
           :else
           {:db ndb})
       {:db ndb}))))

(reg-event-fx
 ::refresh-comp
 [log-event]
 (fn [{:keys [db]} [_ {:keys [composition] :as inp}]]
   (let [comp (db/add-indexes inp)
         lyrics? (> (->> comp :noteseq (map :lyrics) (filter identity) count) 0)
         ndb (-> db
                 (update-in [:composition] (constantly comp))
                 (update-in [:props :show-lyrics] (constantly lyrics?))
                 (update-in [:props :cursor-pos]
                            (constantly
                             (let [in (-> comp :index last)]
                               (zipmap [:row-index :bhaag-index :note-index :nsi] in)))))]
     {:db ndb
      :dispatch [::set-active-panel :home-panel]})))

(reg-event-fx
 ::init-audio-ctx
 (fn [{:keys [db]} [_ audctx]]
   {:db (if-not (:audio-context db)
          (assoc db :audio-context audctx)
          db)}))

(reg-event-fx
 ::show-lyrics?
 (fn [{:keys [db]} [_ ival]]
   {:db (update-in db [:props :show-lyrics] (constantly ival))}))

(reg-event-fx
 ::newline-on-avartan?
 (fn [{:keys [db]} [_ ival]]
   {:db (update-in db [:props :newline-on-avartan?] (constantly ival))}))

(reg-event-fx
 ::set-bpm
 [log-event]
 (fn [{:keys [db]} [_ ival]]
   {:db (update-in db [:props :bpm] (constantly ival))}))

(reg-event-fx
 ::beat-mode
 (fn [{:keys [db]} [_ ival]]
   {:db (update-in db [:props :beat-mode] (constantly ival))}))

(reg-event-fx
 ::tanpura?
 (fn [{:keys [db]} [_ ival]]
   {:db (update-in db [:props :tanpura?] (constantly ival))}))

(reg-event-fx
 ::play
 [log-event]
 (fn [{:keys [db]} [_ _]]
   (if-not (:audio-context db)
     {:dispatch [::init-audio-buffers]}
     (let [bpm (-> db :props :bpm)
           note-interval (/ 60 bpm)
           {:keys [audio-context clock]} db
           now (.-currentTime audio-context)
           taal (-> db :composition :taal)
           num-beats (:num-beats (taal-def taal))
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
                         notseq)))
                (reduce into []))
           num-notes (count a1)
           metro-tick-seq
           (->> db :composition :noteseq
                (map vector (range 0 (->> db :composition :noteseq count inc) note-interval)
                     (range))
                (map (fn[[at note-index {:keys [notes] :as ivec}]]
                       (let [note-index (mod (inc note-index) (-> taal-def taal :num-beats))]
                         (if (and (-> db :props :beat-mode (= :metronome))
                                  (metronome-on-at note-index))
                           [[:tick2 (+ now at) note-interval]]
                           []))))
                (reduce into []))
           taal-len-in-secs (* num-beats note-interval)
           num-cycles (inc (int (/ (->> db :composition :noteseq count dec) num-beats)))
           tabla-beat-seq
           (mapv #(vector
                   (-> (str (name taal) bpm "bpm") keyword)
                   (+ now (* taal-len-in-secs %)) taal-len-in-secs)
                 (range num-cycles))
           ;;find note indexes where duration should be long if followed by avagraha
           ;;returns a list of 2-tuples, where first is index and second is duration of note
           avagraha-note-indexes
           (->> a1
                (map vector (range))
                reverse
                (reduce
                 (fn [iacc [indx [inote iat idur :as in0]]]
                   (if (= inote [:madhyam :a])
                     (-> iacc
                         (update-in [:indx] (constantly indx))
                         (update-in [:duracc] + idur))
                     (if (> (-> iacc :duracc) 0)
                       (-> iacc
                           (update-in [:acc]
                                      (fn[j]
                                        (into j [(mapv iacc [:indx :duracc])])))
                           (assoc :indx nil :duracc 0))
                       iacc)))
                 {:indx nil :duracc 0 :acc []})
                :acc
                (map (fn[[a b]] [(dec a) b])))
           ;;update the noteindex from the previous var to have longer durations
           a1 (->> (reduce (fn[acc i]
                             (update-in acc [(first i)] (fn[[a b c]] [a b (+ c (second i))])))
                           a1 avagraha-note-indexes)
                   (into (if (-> db :props :beat-mode (= :tabla))
                           tabla-beat-seq metro-tick-seq))
                   (sort-by second))
           a1 (if (-> db :props :tanpura?)
                (let [last-note-time (- (-> a1 last second) now )
                      sample-len 3
                      tanpura-pitch (-> db :props :pitch)
                      ;;length of sample is 4 secs
                      play-n-times (int (/ last-note-time sample-len))
                      conj-vec (mapv
                                #(vector (keyword tanpura-pitch)
                                         (+ now (* % sample-len)) sample-len {:gain 0.5})
                                (range (inc play-n-times)))]
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
                   :play-note-index 0
                   :note-interval note-interval
                   :num-notes num-notes
                   ;;translates the play-note index to the view-note index
                   :play-to-view-map a2
                   :timer
                   (-> (c/set-timeout! clock #(dispatch [::clock-tick-event]) 0)
                       (c/repeat! 400)))
        :dispatch [::clock-tick-event]}))))

(reg-event-fx
 ::pause
 [log-event]
 (fn [{:keys [db]} [_ _]]
   (c/clear! (:timer db))
   {:db (-> (assoc db :play-state :pause)
            (dissoc :timer))}))

(reg-event-fx
 ::stop
 [log-event]
 (fn [{:keys [db]} [_ _]]
   (c/clear! (:timer db))
   {:db (-> (assoc db :play-state :stop)
            (dissoc :timer)
            (assoc :play-note-index 0))}))

(reg-event-fx
 ::register-elem
 (fn [{:keys [db]} [_ index nsi elem]]
   {:db
    (if (= 0 index)
      (update-in db [:elem-index ] (constantly [elem]))
      (update-in db [:elem-index ] conj elem))}))

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
                              play-note-index)
                            scroll-fn
                            (fn[indx]
                              (let [[inote iat idur :as noteat] (play-at-time indx)
                                    view-note-index ((:play-to-view-map db) indx)]
                                (when view-note-index
                                  (let [notel (get-in (:elem-index db) [view-note-index])
                                        bcr (.getBoundingClientRect notel)]
                                    (js/setTimeout
                                     (fn[]
                                       (do
                                         (let [mnotes-div (:music-notes-element db)
                                               cur-sctop (.-scrollTop mnotes-div)
                                               last-y (.-y bcr)
                                               ch (.-clientHeight mnotes-div)
                                               htdiff (- last-y cur-sctop)
                                               perc (/ htdiff ch)]
                                           (when (> perc 0.2)
                                             (let [inc-by (* 0.2 htdiff)]
                                               (.scrollTo
                                                mnotes-div
                                                (clj->js {"top" (+ inc-by cur-sctop)
                                                          "behavior" "smooth"}))))
                                           ;;at the end, scroll to top
                                           (when (= (dec (:num-notes db)) view-note-index)
                                             (.scrollTo mnotes-div
                                                        (clj->js {"top" 0 "behavior"
                                                                  "smooth"}))))))
                                     600)))))]
                        (->> past-notes-to-play
                             (mapv (fn[ indx]
                                     (let [[inote iat idur :as noteat] (play-at-time indx)
                                           iat (- iat at)
                                           view-note-index ((:play-to-view-map db) indx)]
                                       (play-sample db [inote (if (> iat 0) iat 0) idur
                                                        (if (= 4 (count noteat))
                                                          (last noteat) {})])
                                       (when view-note-index
                                         (let [notel (get-in (:elem-index db) [view-note-index])]
                                           (js/setTimeout
                                            (fn []
                                              (set! (.-style notel)
                                                    (str "fill-opacity:0.2"))
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
                                                  (set! (.-style notel)
                                                        (str "fill-opacity:0"))))))
                                            (* 1000 iat))))))))
                        (->> past-notes-to-play last scroll-fn)
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
 ::set-music-notes-element
 (fn [{:keys [db]} [_ elem]]
   {:db (assoc db :music-notes-element elem)}))

(reg-event-fx
 ::set-font-size
 (fn [{:keys [db]} [_ font-size]]
   {:db (update-in db [:dispinfo :font-size] (constantly font-size))}))

#_(reg-event-fx
 ::pitch-shift
 (fn [{:keys [db]} [_ shift-by]]
   @(:sample-buffers db)
   {:db (update-in db [:dispinfo :font-size] (constantly font-size))}))

#_(reg-event-fx
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
