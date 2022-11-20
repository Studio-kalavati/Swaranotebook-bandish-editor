(ns bhatkhande-editor.db
  (:require
   [sargam.spec :as us]
   [sargam.talas :as talas
    :refer [teentaal jhaptaal ektaal rupak dadra kehrwa]]))

(defn get-noteseq-index
  "given a multi-index of row,bhaag and note,
  returns the index of the note in m-noteseq.  "
  [{:keys [row-index bhaag-index note-index] :as click-index} taal-def]
  (let [num-beats (:num-beats taal-def)
        a1 (* row-index num-beats)
        a2 (apply + (take bhaag-index (:bhaags taal-def)))]
    (+ a1 a2 note-index)))

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
        res
        {:m-noteseq m-noteseq
         :taal teentaal
         :indexed-noteseq (split-bhaags m-noteseq teentaal)}]
    res))

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


(def default-edit-props {:raga :todi :octave-mode :lower
                         :note-pos {}
                         :note-index []})
(def default-db
  {
   ;;properties for display of current part, from bhatkhande editor
   :swaras-changed true
   :init-state {:cursor-color 0}
   :dispinfo (merge dispinfo m-dispinfo)
   :m-dispinfo m-dispinfo
   :dim {:editor (mapv dispinfo [:x-end :y-end])}

   ;;properties for this application
   :composition init-comp
   :comp-metadata {:default {:name "Todi "}}
   :edit-props default-edit-props 
   :user-state {}
   :language :hindi
   :state :waiting-for-event
   })
