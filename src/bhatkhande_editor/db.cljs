(ns bhatkhande-editor.db
  (:require
   [sargam.spec :as us]
   [sargam.talas :as talas
    :refer [teentaal jhaptaal ektaal rupak dadra kehrwa]]))

(def init-comp
  (let [m-noteseq
        [
         [{:note [:madhyam :s]}]
         [{:note [:madhyam :r]}]
         [{:note [:madhyam :g]}
          {:note [:madhyam :m]}]
         [{:note [:madhyam :m]}]

         [{:note [:madhyam :g]}
          {:note [:taar :s]}
          {:note [:madhyam :m]}]
         [{:note [:madhyam :p]}]
         [{:note [:madhyam :d]}]
         [{:note [:madhyam :n]}]

         [{:note [:taar :s]}]
         [{:note [:madhyam :s]}]
         [{:note [:madhyam :r]}]
         [{:note [:madhyam :g]}]
         [{:note [:madhyam :m]}]
         [{:note [:madhyam :p]}]
         [{:note [:madhyam :d]}]
         [{:note [:madhyam :n]}]
         [{:note [:madhyam :g]}
          {:note [:taar :s]}
          {:note [:madhyam :m]}]
         [{:note [:taar :s]}]
         [{:note [:madhyam :s]}]
         [{:note [:madhyam :r]}]
         [{:note [:madhyam :g]}]
         [{:note [:madhyam :m]}]

         ]]
    {:m-noteseq m-noteseq
     :taal teentaal}))

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
                         :note-pos {}})
(def default-db
  {
   ;;properties for display of current part, from bhatkhande editor
   :cursor {:cursor-index [0]
            :cursor-position [-1 -1]}
   :swaras-changed true
   :draw-cursor? true
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
