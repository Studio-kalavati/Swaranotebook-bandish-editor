(ns bhatkhande-editor.db
  (:require
   [sargam.spec :as us]
   [clojure.zip :as z]
   [clojure.walk :as w]
   [sargam.talas :as talas
    :refer [taal-def]]))

(defn get-noteseq-index
  "given a multi-index of row,bhaag and note,
  returns the index of the note in m-noteseq.  "
  [{:keys [row-index bhaag-index note-index] :as click-index} taal-def]
  (let [num-beats (:num-beats taal-def)
        a1 (* row-index num-beats)
        a2 (apply + (take bhaag-index (:bhaags taal-def)))]
    (+ a1 a2 note-index)))

(defn make-index-seq
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
                         (map vector (range))
                         (reduce
                          (fn[acc1 [ni i]]
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
  [noteseq taal-def]
  (->> noteseq
       (partition-all (-> taal-def :num-beats))
       (map vec)
       (mapv (fn[i]
               (let [{:keys [fin acc] :as ac}
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
  [indexed]
  (let [index-order (make-index-seq indexed)
        index-forward-seq (zipmap (subvec index-order 0 (count index-order))
                                  (vec (rest index-order)))
        index-backward-seq (zipmap (vec (rest index-order))
                                   (subvec index-order 0 (count index-order)) )]
    [index-forward-seq
     index-backward-seq]))

(def init-comp
    (let [m-noteseq
          [
           [{:note [:mandra :s]}]
           [{:note [:mandra :r]}]
           [{:note [:mandra :g]}
            {:note [:mandra :m]}]
           [{:note [:mandra :m+]}]

           [{:note [:mandra :p]}
            {:note [:mandra :-d]}
            {:note [:mandra :d]}]

           [{:note [:mandra :-n]}]
           [{:note [:mandra :n]}]

           [{:note [:madhyam :s]}]
           [{:note [:madhyam :-r]}]
           [{:note [:madhyam :r]}]
           [{:note [:madhyam :-g]}]
           [{:note [:madhyam :g]}]
           [{:note [:madhyam :m]}]
           [{:note [:madhyam :m+]}]
           [{:note [:madhyam :p]}]
           [{:note [:madhyam :-d]}]
           [{:note [:madhyam :d]}]
           [
            {:note [:madhyam :-n]}
            {:note [:madhyam :n]}
            {:note [:taar :s]}]
           [{:note [:taar :-r]}]
           [{:note [:taar :r]}]
           [{:note [:taar :-g]}]
           [{:note [:taar :g]}]
           [{:note [:taar :m]}]
           ]
          taal-id :teentaal
          cur-taal (taal-def taal-id)
          indexed (split-bhaags m-noteseq cur-taal)
          [f b] (get-forward-backward-map indexed)
          res
          {:m-noteseq m-noteseq
           :taal taal-id}]
      res))

(defn add-indexes
  [comp]
  (let [{:keys [taal m-noteseq] :as imap} comp
        cur-taal (taal-def taal)
        indexed (split-bhaags m-noteseq cur-taal)
        [f b] (get-forward-backward-map indexed)]
    (assoc imap
           ;;the same m-noteseq that is split into groups of rows (one per taal cycle)
           ;;further into bhaags per row (e.g. 4 in teentaal)
           ;;further into notes and sub-notes
           :indexed-noteseq indexed
           ;;a map where the key is a 4-part index [row bhaag note sub-note]
           ;;and the value is the next note in sequence (also expressed as a 4-part index)
           :index-forward-seq f
           :index-backward-seq b)))

(def mswaras (subvec us/i-note-seq 0 (- (count us/i-note-seq) 2)))

(def note-seq (vec (for [octave (range 1 8)
                         i ["c" "c#" "d" "d#" "e" "f" "f#" "g" "g#" "a" "a#" "b"]]
                     [i octave])))

(def image-map (zipmap (conj (vec (for [i [:mandra :madhyam :taar] j (take 12 us/i-note-seq)]
                                    [i j])) [:ati-taar :s])
                       (mapv
                        #(str "/images/swaras/hindi/png/" % ".png" )
                                        ;#(str "/images/swaras/hindi/svg/Vector_" % ".svg" )
                        (range 1 38))))
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
   :font-size 20 :spacing 10 :text-align :left})


(def default-edit-props {:raga :todi
                         :note-pos {}
                         :cursor-pos (-> init-comp :index-seq count)
                         :note-index []})
(def default-db
  {
   :init-state {:cursor-color 0}
   :dispinfo (merge dispinfo m-dispinfo)
   :m-dispinfo m-dispinfo
   :dim {:editor (mapv dispinfo [:x-end :y-end])}

   ;;properties for this application
   :composition (add-indexes init-comp)
   :edit-props default-edit-props
   :language :hindi
   })
