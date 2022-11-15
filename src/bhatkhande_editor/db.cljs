(ns bhatkhande-editor.db
  (:require [sargam.spec :as us]))

(def init-comp
  {:default {:part-1 {:taal :teentaal, :swaras [[{:swara "-", :enabled true, :x :x1, :cp [0 0]}] [{:swara "-", :enabled true, :x :x1, :cp [1 0]}] [{:swara "-", :enabled true, :x :x1, :cp [2 0]}] [{:swara "-", :enabled true, :x :x1, :cp [3 0]}] [{:swara "-", :enabled true, :x :x1, :cp [4 0]}] [{:swara "-", :enabled true, :x :x1, :cp [5 0]}] [{:swara "सा", :enabled true, :saptak :madhyam, :x :x1, :cp [6 0]}] [{:swara "रे॒", :enabled true, :saptak :madhyam, :x :x1, :cp [7 0]}] [{:swara "ग॒", :enabled true, :saptak :madhyam, :x :x1, :cp [8 0]}] [{:swara "रे॒", :enabled true, :saptak :madhyam, :x :x1, :cp [9 0]}] [{:swara "ग॒", :enabled true, :saptak :madhyam, :x :x2, :cp [10 0]} {:swara "रे॒", :enabled true, :saptak :madhyam, :x :x2, :cp [10 1]}] [{:swara "म॑", :enabled true, :saptak :madhyam, :x :x1, :cp [11 0]}] [{:swara -, :enabled true, :x :x1, :cp [12 0]}] [{:swara "प", :enabled true, :saptak :madhyam, :x :x1, :cp [13 0]}] [{:swara "ध॒", :enabled true, :saptak :madhyam, :x :x1, :cp [14 0]}] [{:swara "सा", :enabled true, :saptak :taar, :x :x1, :cp [15 0]}] [{:swara "सा", :enabled true, :saptak :taar, :x :x1, :cp [16 0]}] [{:swara "सा", :enabled true, :saptak :taar, :x :x1, :cp [17 0]}]], :cursor-position [25 0], :x-switch :x1, :octave-switch :lower, :part-name 1, :raga :bilawal}}})

(def mswaras (subvec us/i-note-seq 0 (- (count us/i-note-seq) 2)))

(def note-seq (vec (for [octave (range 1 8)
                         i ["c" "c#" "d" "d#" "e" "f" "f#" "g" "g#" "a" "a#" "b"]]
                     [i octave])))

(def image-map (zipmap (conj (vec (for [i [:mandra :madhyam :taar] j (take 12 us/i-note-seq)]
                                    [i j])) [:ati-taar :s])
                       (mapv
                        #(str "/images/swaras/hindi/png/Vector_" % ".png" )
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


(def default-edit-props {:raga :bilawal :octave-mode :lower})
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
   :compositions init-comp
   :composition-name :default
   :part-name :part-1
   :composition-id :default
   :comp-metadata {:default {:name "Todi "}}
   :edit-props default-edit-props 
   :user-state {}
   :language :hindi
   :state :waiting-for-event
   })
