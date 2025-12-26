(ns bhatkhande-editor.db
  (:require
   [sargam.spec :as us]
   [sargam.talas :as talas :refer [taal-def]])
  (:require-macros [adzerk.env :as env]))

(env/def
  apiKey :required
  authDomain :required
  projectId :required
  storageBucket :required
  messagingSenderId :required
  appId :required
  posthogKey :required
  mailerliteApiToken :required
  mailerliteGroupId :required)

(defn get-bandish-url
  [path]
  (str "https://storage.googleapis.com/" storageBucket "/" path))

(defn get-long-url
  [path]
  (str (.-origin (.-location js/window)) "/app/view/" path))

(defn get-noteseq-index
  "given a multi-index of row,bhaag and note,
  returns the index of the note in noteseq.  "
  [{:keys [avartan-index bhaag-index note-index] :as inp} taal]
  (let [td (taal-def taal)
        num-beats (:num-beats td)
        a1 (* avartan-index num-beats)
        a2 (apply + (take bhaag-index (:bhaags td)))
        res (+ a1 a2 note-index)]
    res))

(defn get-avartan-index
  "Assume a comp has 4 avartans in Teentaal.
  Return a seq of 0,16, 32, 48 indicate the note numbers if they were played in succession. "
  [{:keys [score-part-index avartan-index bhaag-index note-index] :as inp} score-parts taal]
  (let [td (taal-def taal)
        num-beats (:num-beats td)
        a1 (* avartan-index num-beats)
        a2 (mapv #(count (:noteseq %))score-parts)
       ;; res (+ a1 a2 note-index)
        ]
    0))

(defn make-index-seq
  "given a indexed sequence of notes, returns a flat sequence where each element is [avartan-index bhaag-index note-index note-sub-index],i.e the note indexes that can be used to retrieve a note  "
  [indexed-ns]
  (let [bi
        (fn[avartan-index bhaag-index note-map-seq]
          (->>
           note-map-seq
           (map vector (range))
           (reduce
            (fn[acc [note-index note]]
              (into acc (->>
                         note
                         :notes
                         (map vector (range))
                         (reduce
                          (fn[acc1 [ni _]]
                            ;;create all notes in a single beat.
                            (let [note-xy-map [avartan-index
                                               bhaag-index
                                               note-index
                                               ni]]
                              (conj acc1 note-xy-map)))
                          []))))
            [])))
        bfn (fn[acci [avartan-index bhaag]]
              (into acci (->> bhaag
                              (map vector (range))
                              (reduce (fn[ acc [indx i]]
                                        (let [indexes (bi avartan-index indx i )]
                                          (into acc indexes))) []))))
        b1
        (->> indexed-ns
             (map vector (range))
             (reduce bfn []))]
    b1))

(defn split-bhaags
  "given a flat sequence of notes, returns a sequence where a specific note can be retrieved with
  (get-in iseq [avartan-index bhaag-index note-index note-sub-index])"
  [noteseq taal-def]
  (->> noteseq
       (partition-all (-> taal-def :num-beats))
       (map vec)
       (mapv (fn[i]
               (let [{:keys [fin _]}
                     (reduce
                      (fn[ac bhaag-len]
                        (let [[a b] (split-at bhaag-len (:acc ac))
                              r1
                              (if (= 0 (count a)) ac
                                  (update-in ac [:fin] conj (vec a)))
                              r2 (update-in r1 [:acc] (constantly (vec b)))]
                          r2))
                      {:fin [] :acc i}
                      (-> taal-def :bhaags))]
                 fin)))))

(defn get-forward-backward-map
  "returns a vector with 2 maps, the first indicating the next note in the sequence, and the second
  indicating the previous note in the sequence.
  Used to pusvara the cursor to the next or previous position while editing.
  "
  [indexed]
  (let [index-order (make-index-seq indexed)
        index-forward-seq (zipmap (subvec index-order 0 (count index-order))
                                  (vec (rest index-order)))
        index-backward-seq (zipmap (vec (rest index-order))
                                   (subvec index-order 0 (count index-order)) )]
    [index-order
     index-forward-seq
     index-backward-seq]))

(defn get-forward-backward-map2
  "returns a vector with 2 maps, the first indicating the next note in the sequence, and the second
  indicating the previous note in the sequence.
  Used to pushruti the cursor to the next or previous position while editing.
  "
  [index-order]
  (let [index-forward-seq (zipmap (subvec index-order 0 (count index-order))
                                  (vec (rest index-order)))
        index-backward-seq (zipmap (vec (rest index-order))
                                   (subvec index-order 0 (count index-order)) )]
    {:index-forward-seq index-forward-seq
     :index-backward-seq index-backward-seq}))

(defn space-notes
  [n]
  (vec (repeat n {:notes [{:svara [:madhyam :_]}]})))

(defn get-sahitya
  [comp {:keys [score-part-index avartan-index bhaag-index]}]
  (let [sahitya (->> (get-in comp [:indexed-noteseq
                                   score-part-index avartan-index bhaag-index]))
        sah-list (when sahitya (mapv :lyrics sahitya))]
    sah-list))

(def test-comp
  {:noteseq
   [{:notes [{:shruti [:madhyam :a], :npb 1}], :lyrics "ae,ri,aa,li"}
    {:notes [{:shruti [:madhyam :s]}]}
    {:notes [{:shruti [:madhyam :r]}]}
    {:notes [{:shruti [:madhyam :g]}]}
    {:notes [{:shruti [:madhyam :m]}], :lyrics "pi,ya,bi,na"}
    {:notes [{:shruti [:madhyam :s], :npb 1}]}
    {:notes [{:shruti [:madhyam :s], :npb 1}]}
    {:notes [{:shruti [:madhyam :s], :npb 1}]}
    {:notes [{:shruti [:madhyam :s], :npb 1}]}
    {:notes [{:shruti [:madhyam :s], :npb 1}]}
    {:notes [{:shruti [:madhyam :r], :npb 1}]}
    {:notes [{:shruti [:madhyam :g], :npb 1}]}
    {:notes [{:shruti [:madhyam :m], :npb 1}]}
    {:notes [{:shruti [:taar :s], :npb 1}]}
    {:notes [{:shruti [:taar :r], :npb 1}]}
    {:notes [{:shruti [:taar :g], :npb 1}]}
    {:notes [{:shruti [:mandra :s], :npb 2} {:shruti [:mandra :r], :npb 2}]}
    {:notes [{:shruti [:mandra :g], :npb 2} {:shruti [:mandra :m], :npb 2}]}
    {:notes
     [{:shruti [:mandra :r], :npb 3}
      {:shruti [:mandra :g], :npb 3}
      {:shruti [:mandra :m], :npb 3}]}
    {:notes
     [{:shruti [:mandra :g], :npb 3}
      {:shruti [:mandra :m], :npb 3}
      {:shruti [:mandra :p], :npb 3}]}
    {:notes
     [{:shruti [:mandra :g], :npb 4}
      {:shruti [:mandra :m], :npb 4}
      {:shruti [:mandra :p], :npb 4}
      {:shruti [:mandra :d], :npb 4}]}
    {:notes [{:shruti [:mandra :s], :npb 1}]}
    {:notes [{:shruti [:madhyam :-]}]}],
   :taal :teentaal})

(defn init-part
  [num-beats ptitle]
  (let [inoteseq
          [
           {:notes [{:svara [:madhyam :s]}] }
           {:notes [{:svara [:madhyam :r]}] }
           {:notes [{:svara [:madhyam :g]}] }
           {:notes [{:svara [:madhyam :m]}] }]
          noteseq (into inoteseq (space-notes (- num-beats (count inoteseq))))]
      {:part-num 3 :part-title ptitle
       :noteseq noteseq}))

(def part-noteseq [
                   {:notes [{:svara [:madhyam :s]}] :lyrics "a"}
                   {:notes [{:svara [:madhyam :r]}] :lyrics "b"}
                   {:notes [{:svara [:madhyam :g]}] :lyrics "c"}
                   {:notes [{:svara [:madhyam :m]}] :lyrics "d"}
                   {:notes [{:svara [:madhyam :p]}] :lyrics "a"}
                   {:notes [{:svara [:madhyam :d]}] :lyrics "b"}
                   {:notes [{:svara [:madhyam :n]}] :lyrics "c"}
                   {:notes [{:svara [:taar :s]}] :lyrics "d"}])

(defn cvt-format
  [input-format]
  (let [resp
        (cond
          (:noteseq input-format)
          {:title "Bandish"
           :score-parts
           [{:part-title "sthayi"
             :noteseq
             (let [inoteseq
                   (->> input-format
                        :noteseq
                        (reduce
                         (fn[{:keys [lyr-acc] :as iacc}
                             {:keys [notes lyrics]:as imap}]
                           (let [lyr (if lyrics (.split lyrics #",") lyr-acc)]
                             (-> iacc
                                 (update-in
                                  [:noteseq]
                                  conj
                                  (let [in0
                                        (assoc {}
                                               :notes
                                               (mapv (fn[n]
                                                       (dissoc (clojure.set/rename-keys n {:shruti :svara})
                                                               :npb)) notes))]
                                    (if (first lyr)
                                      (assoc in0 :lyrics (first lyr))
                                      in0)))
                                 (update-in [:lyr-acc] (constantly (rest lyr))))))
                         {:noteseq [] :lyr-acc []})
                        :noteseq)]
               (into inoteseq (space-notes (- (:num-beats (taal-def (:taal input-format)))
                                              (count inoteseq)))))}]
            :taal (:taal input-format)}
          (:score-parts input-format) input-format
          :else "invalid format")]
    resp))

(def init-comp
    (let [inoteseq part-noteseq
          taal-id :teentaal
          noteseq
          (into inoteseq (space-notes (- (:num-beats (taal-def taal-id))
                                         (count inoteseq))))
          res
          {:score-parts [{:part-title "sthayi"
                          :noteseq noteseq}
                         {:part-title "antara"
                          :noteseq (into (vec (reverse inoteseq) )
                                         (space-notes (- (:num-beats (taal-def taal-id))
                                                         (count inoteseq))))}]
           :title "Bandish name"
           :taal taal-id}]
      res))

(defn add-part-index
  [taal {:keys [noteseq] :as imap}]
  (let [cur-taal (taal-def taal)
        indexed (split-bhaags noteseq  cur-taal)
        [order f b] (get-forward-backward-map indexed)
        ]
    (assoc {}
           ;;the same noteseq that is split into groups of rows (one per taal cycle)
           ;;further into bhaags per row (e.g. 4 in teentaal)
           ;;further into notes and sub-notes
           :indexed-noteseq indexed
           ;;a map where the key is a 4-part index [row bhaag note sub-note]
           ;;and the value is the next note in sequence (also expressed as a 4-part index)
           :index order
           :index-forward-seq f
           :index-backward-seq b)))

(defn add-indexes
  [{:keys [taal score-parts] :as score}]
  (let [indexes (mapv #(add-part-index taal  %) score-parts)
        index (->> (map :index indexes)
                   (map-indexed (fn[indx item] (mapv #(vec (cons indx  %)) item)))
                   (reduce into))
        indexed-noteseq (mapv :indexed-noteseq indexes)]
    (merge (assoc score :index index :indexed-noteseq indexed-noteseq)
           (get-forward-backward-map2 index))))

(def mswaras (subvec us/i-note-seq 0 (- (count us/i-note-seq) 2)))

(def note-seq (vec (for [octave (range 1 8)
                         i ["c" "c#" "d" "d#" "e" "f" "f#" "g" "g#" "a" "a#" "b"]]
                     [i octave])))

(defn image-map
  [lang]
  (->
   (zipmap (conj (vec (for [i [:mandra :madhyam :taar] j (take 12 us/i-note-seq)]
                        [i j])) [:ati-taar :s])
           (mapv
            #(str "/images/swaras/" lang "/png/" % ".png" )
            (range 1 38)))
   (assoc [:madhyam :a] (str "/images/swaras/common/png/avagraha.png"))))

(defn percentage-95
  [i]
  (let [ iw (js/parseInt i)]
    (- iw (* 0.05 iw))))

(def m-dispinfo {:y 40})
(def dispinfo
  {:x 5  :under 30
   :x-start 5
   :y-inc 80
   :x-end (percentage-95 (try (.-innerWidth js/window) (catch js/Error e 400)))
   :y-end (* 0.6 (try (.-innerHeight js/window) (catch js/Error e 800)))
   :over 30
   :kan {:kan-raise 10
         :reduce-font-size 5
         :reduce-spacing 3
         :reduce-octave-size 5}
   :octave 15
   :part-header-font-size 30
   :comp-label-font-size 35
   :header-y-spacing 50
   :debug {:disp-swara false}
   :cursor-padding 5
   :sam-khaali 35
   :font-size 32 :spacing 10 :text-align :left})

(def default-props {:raga :bilawal
                     :note-pos {}
                     :mode :edit
                     :currently-editing :svaras
                     :lang :english
                     :newline-on-avartan? false
                     :show-lyrics false
                     :bpm 120
                     :beat-mode :metronome
                     :note-octave :madhyam
                     :onscreen-keyboard :show
                     :highlighted-pos []
                     :pitch "c"
                     :tanpura? true
                     :notes-per-beat 1
                     :hidden-parts #{}
                     :note-index []
                     :youtube-sync true})

(def pitch-sharps-list ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"])
(def pitch-s-list ["c" "cs" "d" "ds" "e" "f" "fs" "g" "gs" "a" "as" "b"])

(def pitch-options-list
  (mapv #(assoc {} :id %1 :label %2 :sample %3) (range 0 12) pitch-sharps-list
        pitch-s-list))
(def cursor-index-keys
  [:score-part-index :avartan-index :bhaag-index :note-index :nsi])

(defn comp-decorator
  [comp0]
  (let [comp (add-indexes comp0)]
    {:composition comp
     :props (update-in
             default-props
             [:cursor-pos]
             (constantly
              (let [in (->> comp :index (drop 8) first)]
                (zipmap cursor-index-keys in ))))}))

(def default-db
  (let [icomp init-comp ;; (cvt-format test-comp)
        idb
        (merge (comp-decorator icomp)
               {:init-state {:cursor-color 0}
                :dispinfo (merge dispinfo m-dispinfo)
                :m-dispinfo m-dispinfo
                ;;for storing svara images to light up
                :elem-index []
                :play-head-position (zipmap cursor-index-keys [0 0 0 0 0])
                :dim {:editor (mapv dispinfo [:x-end :y-end])}})]
    idb))

;;[{:part-title sthayi, :noteseq [{:notes [{:svara [:madhyam :s]}], :lyrics a} {:notes [{:svara [:madhyam :r]}], :lyrics b} {:notes [{:svara [:madhyam :g]}], :lyrics c} {:notes [{:svara [:madhyam :m]}], :lyrics d} {:notes [{:svara [:madhyam :p]}], :lyrics a} {:notes [{:svara [:madhyam :d]}], :lyrics b} {:notes [{:svara [:madhyam :n]}], :lyrics c} {:notes [{:svara [:taar :s]}], :lyrics d} {:notes [{:svara [:madhyam :_]}]} {:notes [{:svara [:madhyam :_]}]} {:notes [{:svara [:madhyam :_]}]} {:notes [{:svara [:madhyam :_]}]} {:notes [{:svara [:madhyam :_]}]} {:notes [{:svara [:madhyam :_]}]} {:notes [{:svara [:madhyam :_]}]} {:notes [{:svara [:madhyam :_]}]}]}]

;;[{:part-title sthayi, :noteseq [{:notes [{:npb 1, :svara [:madhyam :a]}], :lyrics ae} {:notes [{:svara [:madhyam :s]}], :lyrics ri} {:notes [{:svara [:madhyam :r]}], :lyrics aa} {:notes [{:svara [:madhyam :g]}], :lyrics li} {:notes [{:svara [:madhyam :m]}], :lyrics pi} {:notes [{:npb 1, :svara [:madhyam :s]}], :lyrics ya} {:notes [{:npb 1, :svara [:madhyam :s]}], :lyrics bi} {:notes [{:npb 1, :svara [:madhyam :s]}], :lyrics na} {:notes [{:npb 1, :svara [:madhyam :s]}]} {:notes [{:npb 1, :svara [:madhyam :s]}]} {:notes [{:npb 1, :svara [:madhyam :r]}]} {:notes [{:npb 1, :svara [:madhyam :g]}]} {:notes [{:npb 1, :svara [:madhyam :m]}]} {:notes [{:npb 1, :svara [:taar :s]}]} {:notes [{:npb 1, :svara [:taar :r]}]} {:notes [{:npb 1, :svara [:taar :g]}]} {:notes [{:npb 2, :svara [:mandra :s]} {:npb 2, :svara [:mandra :r]}]} {:notes [{:npb 2, :svara [:mandra :g]} {:npb 2, :svara [:mandra :m]}]} {:notes [{:npb 3, :svara [:mandra :r]} {:npb 3, :svara [:mandra :g]} {:npb 3, :svara [:mandra :m]}]} {:notes [{:npb 3, :svara [:mandra :g]} {:npb 3, :svara [:mandra :m]} {:npb 3, :svara [:mandra :p]}]} {:notes [{:npb 4, :svara [:mandra :g]} {:npb 4, :svara [:mandra :m]} {:npb 4, :svara [:mandra :p]} {:npb 4, :svara [:mandra :d]}]} {:notes [{:npb 1, :svara [:mandra :s]}]} {:notes [{:svara [:madhyam :-]}]}]}]
