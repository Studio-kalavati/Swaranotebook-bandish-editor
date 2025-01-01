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
  [{:keys [row-index bhaag-index note-index] :as inp} taal]
  (let [td (taal-def taal)
        num-beats (:num-beats td)
        a1 (* row-index num-beats)
        a2 (apply + (take bhaag-index (:bhaags td)))
        res (+ a1 a2 note-index)]
    res))

(defn make-index-seq
  "given a indexed sequence of notes, returns a flat sequence where each element is [row-index bhaag-index note-index note-sub-index],i.e the note indexes that can be used to retrieve a note  "
  [indexed-ns]
  (let [bi
        (fn[row-index bhaag-index note-map-seq]
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
                            (let [note-xy-map [row-index
                                               bhaag-index
                                               note-index
                                               ni]]
                              (conj acc1 note-xy-map)))
                          []))))
            [])))
        bfn (fn[acci [row-index bhaag]]
              (into acci (->> bhaag
                              (map vector (range))
                              (reduce (fn[ acc [indx i]]
                                        (let [indexes (bi row-index indx i )]
                                          (into acc indexes))) []))))
        b1
        (->> indexed-ns
             (map vector (range))
             (reduce bfn []))]
    b1))

(defn split-bhaags
  "given a flat sequence of notes, returns a sequence where a specific note can be retrieved with
  (get-in iseq [row-index bhaag-index note-index note-sub-index])"
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
  Used to pushruti the cursor to the next or previous position while editing.
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

(def test-comp
(let [noteseq
          [
           {:notes [{:shruti [:mandra :s]}]}
           {:notes [{:shruti [:mandra :r]}]}
           {:notes [{:shruti [:mandra :g]}]}
           {:notes [{:shruti [:mandra :m]}]}
           {:notes [{:shruti [:mandra :p]}]}
           {:notes [{:shruti [:mandra :d]}]}
           {:notes [{:shruti [:mandra :n]}]}
           {:notes [{:shruti [:madhyam :s]}]}
           {:notes [{:shruti [:madhyam "-"]}]}
           {:notes [{:shruti [:madhyam "s"]}]}
           {:notes [{:shruti [:madhyam :s]} {:shruti [:madhyam :r]} {:shruti [:madhyam :g]}]}
           {:notes [{:shruti [:madhyam :m]} {:shruti [:madhyam :p]}]}
           {:notes [{:shruti [:madhyam :d]}]}
           {:notes [{:shruti [:madhyam :-n]}]}
           {:notes [{:shruti [:taar :s]}]}
           {:notes [{:shruti [:taar :r]}]}
           {:notes [{:shruti [:taar :g]}]}
           {:notes [{:shruti [:taar :m]}]}
           {:notes [{:shruti [:taar :p]}]}
           {:notes [{:shruti [:taar :d]}]}
           {:notes [{:shruti [:taar :n]}]}
           {:notes [{:shruti [:madhyam :-]}]}
           {:notes [{:shruti [:madhyam :-]}]}
           ]
          taal-id :teentaal
          res
          {:noteseq noteseq
           :taal taal-id}]
      res))

(def init-comp
    (let [noteseq
          [
           {:notes [{:shruti [:madhyam :s]}]}
           {:notes [{:shruti [:madhyam :r]}]}
           {:notes [{:shruti [:madhyam :g]}]}
           {:notes [{:shruti [:madhyam :m]}]}
           {:notes [{:shruti [:madhyam :-]}]}
           ]
          taal-id :teentaal
          res
          {:noteseq noteseq
           :taal taal-id}]
      res))

(defn add-indexes
  [comp]
  (let [{:keys [taal noteseq] :as imap} comp
        cur-taal (taal-def taal)
        indexed (split-bhaags noteseq cur-taal)
        [order f b] (get-forward-backward-map indexed)]
    (assoc imap
           ;;the same noteseq that is split into groups of rows (one per taal cycle)
           ;;further into bhaags per row (e.g. 4 in teentaal)
           ;;further into notes and sub-notes
           :indexed-noteseq indexed
           ;;a map where the key is a 4-part index [row bhaag note sub-note]
           ;;and the value is the next note in sequence (also expressed as a 4-part index)
           :index order
           :index-forward-seq f
           :index-backward-seq b)))

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
   :x-end (percentage-95 (.-innerWidth js/window))
   :y-end (* 0.6 (.-innerHeight js/window))
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
                    :lang :english
                    :newline-on-avartan? false
                    :show-lyrics false
                    :bpm 120
                    :beat-mode :metronome
                    :note-octave :madhyam
                    :keyboard-mode :onscreen-kbd
                    :highlighted-pos []
                    :pitch "c"
                    :tanpura? true
                    :notes-per-beat 1
                    :note-index []})

(def pitch-sharps-list ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"])
(def pitch-s-list ["c" "cs" "d" "ds" "e" "f" "fs" "g" "gs" "a" "as" "b"])

(def pitch-options-list
  (mapv #(assoc {} :id %1 :label %2 :sample %3) (range 0 12) pitch-sharps-list
        pitch-s-list))

(defn comp-decorator
  [comp0]
  (let [comp (add-indexes comp0)]
    {:composition comp
     :props (update-in
             default-props
             [:cursor-pos]
             (constantly
              (let [in (-> comp :index last)]
                (zipmap [:row-index :bhaag-index :note-index :nsi] in))))}))


(def default-db
  (merge (comp-decorator init-comp)
         {:init-state {:cursor-color 0}
          :dispinfo (merge dispinfo m-dispinfo)
          :m-dispinfo m-dispinfo
          ;;for storing svara images to light up
          :elem-index []
          :play-head-position 0
          :dim {:editor (mapv dispinfo [:x-end :y-end])}}))
