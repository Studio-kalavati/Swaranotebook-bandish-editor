(ns bhatkhande-editor.db-test
  (:require [cljs.test :refer (deftest is)]
            [bhatkhande-editor.utils :as ut]
            [sargam.talas :as talas :refer [taal-def]]
            [bhatkhande-editor.db :as db :refer
             [split-bhaags init-comp add-part-index add-indexes]]
            [clojure.data :refer [diff]]
            [clojure.spec.alpha :as s]))

(deftest split-bhaags-test
  (is
   (=
    [[[{:notes [{:svara [:madhyam :s]}], :lyrics "a"}
       {:notes [{:svara [:madhyam :r]}], :lyrics "b"}
       {:notes [{:svara [:madhyam :g]}], :lyrics "c"}
       {:notes [{:svara [:madhyam :m]}], :lyrics "d"}]
      [{:notes [{:svara [:madhyam :p]}], :lyrics "a"}
       {:notes [{:svara [:madhyam :d]}], :lyrics "b"}
       {:notes [{:svara [:madhyam :n]}], :lyrics "c"}
       {:notes [{:svara [:taar :s]}], :lyrics "d"}]
      [{:notes [{:svara [:madhyam :_]}]}
       {:notes [{:svara [:madhyam :_]}]}
       {:notes [{:svara [:madhyam :_]}]}
       {:notes [{:svara [:madhyam :_]}]}]
      [{:notes [{:svara [:madhyam :_]}]}
       {:notes [{:svara [:madhyam :_]}]}
       {:notes [{:svara [:madhyam :_]}]}
       {:notes [{:svara [:madhyam :_]}]}]]]
    (split-bhaags (-> init-comp :score-parts first :noteseq) (taal-def :teentaal)))))

(deftest add-part-index-test
  (is (=
       {:indexed-noteseq
        [[[{:notes [{:svara [:madhyam :s]}], :lyrics "a"}
           {:notes [{:svara [:madhyam :r]}], :lyrics "b"}
           {:notes [{:svara [:madhyam :g]}], :lyrics "c"}
           {:notes [{:svara [:madhyam :m]}], :lyrics "d"}]
          [{:notes [{:svara [:madhyam :p]}], :lyrics "a"}
           {:notes [{:svara [:madhyam :d]}], :lyrics "b"}
           {:notes [{:svara [:madhyam :n]}], :lyrics "c"}
           {:notes [{:svara [:taar :s]}], :lyrics "d"}]
          [{:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}]
          [{:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}]]],
        :index
        [[0 0 0 0]
         [0 0 1 0]
         [0 0 2 0]
         [0 0 3 0]
         [0 1 0 0]
         [0 1 1 0]
         [0 1 2 0]
         [0 1 3 0]
         [0 2 0 0]
         [0 2 1 0]
         [0 2 2 0]
         [0 2 3 0]
         [0 3 0 0]
         [0 3 1 0]
         [0 3 2 0]
         [0 3 3 0]],
        :index-forward-seq
        {[0 2 2 0] [0 2 3 0],
         [0 1 0 0] [0 1 1 0],
         [0 2 0 0] [0 2 1 0],
         [0 0 1 0] [0 0 2 0],
         [0 0 3 0] [0 1 0 0],
         [0 1 2 0] [0 1 3 0],
         [0 3 1 0] [0 3 2 0],
         [0 2 1 0] [0 2 2 0],
         [0 0 0 0] [0 0 1 0],
         [0 1 3 0] [0 2 0 0],
         [0 2 3 0] [0 3 0 0],
         [0 0 2 0] [0 0 3 0],
         [0 1 1 0] [0 1 2 0],
         [0 3 2 0] [0 3 3 0],
         [0 3 0 0] [0 3 1 0]},
        :index-backward-seq
        {[0 2 2 0] [0 2 1 0],
         [0 1 0 0] [0 0 3 0],
         [0 2 0 0] [0 1 3 0],
         [0 0 1 0] [0 0 0 0],
         [0 3 3 0] [0 3 2 0],
         [0 0 3 0] [0 0 2 0],
         [0 1 2 0] [0 1 1 0],
         [0 3 1 0] [0 3 0 0],
         [0 2 1 0] [0 2 0 0],
         [0 1 3 0] [0 1 2 0],
         [0 2 3 0] [0 2 2 0],
         [0 0 2 0] [0 0 1 0],
         [0 1 1 0] [0 1 0 0],
         [0 3 2 0] [0 3 1 0],
         [0 3 0 0] [0 2 3 0]}})
      (add-part-index :teentaal (first (:score-parts init-comp)))))

(deftest add-indexes-test
  (is (=
       {:score-parts
        [{:part-num 0,
          :part-title "sthayi",
          :noteseq
          [{:notes [{:svara [:madhyam :s]}], :lyrics "a"}
           {:notes [{:svara [:madhyam :r]}], :lyrics "b"}
           {:notes [{:svara [:madhyam :g]}], :lyrics "c"}
           {:notes [{:svara [:madhyam :m]}], :lyrics "d"}
           {:notes [{:svara [:madhyam :p]}], :lyrics "a"}
           {:notes [{:svara [:madhyam :d]}], :lyrics "b"}
           {:notes [{:svara [:madhyam :n]}], :lyrics "c"}
           {:notes [{:svara [:taar :s]}], :lyrics "d"}
           {:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}]}
         {:part-num 1,
          :part-title "antara",
          :noteseq
          [{:notes [{:svara [:taar :s]}], :lyrics "d"}
           {:notes [{:svara [:madhyam :n]}], :lyrics "c"}
           {:notes [{:svara [:madhyam :d]}], :lyrics "b"}
           {:notes [{:svara [:madhyam :p]}], :lyrics "a"}
           {:notes [{:svara [:madhyam :m]}], :lyrics "d"}
           {:notes [{:svara [:madhyam :g]}], :lyrics "c"}
           {:notes [{:svara [:madhyam :r]}], :lyrics "b"}
           {:notes [{:svara [:madhyam :s]}], :lyrics "a"}
           {:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}
           {:notes [{:svara [:madhyam :_]}]}]}],
        :title "Bandish name",
        :taal :teentaal,
        :index
        [[0 0 0 0 0]
         [0 0 0 1 0]
         [0 0 0 2 0]
         [0 0 0 3 0]
         [0 0 1 0 0]
         [0 0 1 1 0]
         [0 0 1 2 0]
         [0 0 1 3 0]
         [0 0 2 0 0]
         [0 0 2 1 0]
         [0 0 2 2 0]
         [0 0 2 3 0]
         [0 0 3 0 0]
         [0 0 3 1 0]
         [0 0 3 2 0]
         [0 0 3 3 0]
         [1 0 0 0 0]
         [1 0 0 1 0]
         [1 0 0 2 0]
         [1 0 0 3 0]
         [1 0 1 0 0]
         [1 0 1 1 0]
         [1 0 1 2 0]
         [1 0 1 3 0]
         [1 0 2 0 0]
         [1 0 2 1 0]
         [1 0 2 2 0]
         [1 0 2 3 0]
         [1 0 3 0 0]
         [1 0 3 1 0]
         [1 0 3 2 0]
         [1 0 3 3 0]],
        :indexed-noteseq
        [[[[{:notes [{:svara [:madhyam :s]}], :lyrics "a"}
            {:notes [{:svara [:madhyam :r]}], :lyrics "b"}
            {:notes [{:svara [:madhyam :g]}], :lyrics "c"}
            {:notes [{:svara [:madhyam :m]}], :lyrics "d"}]
           [{:notes [{:svara [:madhyam :p]}], :lyrics "a"}
            {:notes [{:svara [:madhyam :d]}], :lyrics "b"}
            {:notes [{:svara [:madhyam :n]}], :lyrics "c"}
            {:notes [{:svara [:taar :s]}], :lyrics "d"}]
           [{:notes [{:svara [:madhyam :_]}]}
            {:notes [{:svara [:madhyam :_]}]}
            {:notes [{:svara [:madhyam :_]}]}
            {:notes [{:svara [:madhyam :_]}]}]
           [{:notes [{:svara [:madhyam :_]}]}
            {:notes [{:svara [:madhyam :_]}]}
            {:notes [{:svara [:madhyam :_]}]}
            {:notes [{:svara [:madhyam :_]}]}]]]
         [[[{:notes [{:svara [:taar :s]}], :lyrics "d"}
            {:notes [{:svara [:madhyam :n]}], :lyrics "c"}
            {:notes [{:svara [:madhyam :d]}], :lyrics "b"}
            {:notes [{:svara [:madhyam :p]}], :lyrics "a"}]
           [{:notes [{:svara [:madhyam :m]}], :lyrics "d"}
            {:notes [{:svara [:madhyam :g]}], :lyrics "c"}
            {:notes [{:svara [:madhyam :r]}], :lyrics "b"}
            {:notes [{:svara [:madhyam :s]}], :lyrics "a"}]
           [{:notes [{:svara [:madhyam :_]}]}
            {:notes [{:svara [:madhyam :_]}]}
            {:notes [{:svara [:madhyam :_]}]}
            {:notes [{:svara [:madhyam :_]}]}]
           [{:notes [{:svara [:madhyam :_]}]}
            {:notes [{:svara [:madhyam :_]}]}
            {:notes [{:svara [:madhyam :_]}]}
            {:notes [{:svara [:madhyam :_]}]}]]]],
        :index-forward-seq
        {[0 0 3 3 0] [1 0 0 0 0],
         [0 0 1 0 0] [0 0 1 1 0],
         [0 0 2 2 0] [0 0 2 3 0],
         [0 0 1 3 0] [0 0 2 0 0],
         [0 0 1 2 0] [0 0 1 3 0],
         [0 0 3 2 0] [0 0 3 3 0],
         [1 0 1 1 0] [1 0 1 2 0],
         [1 0 2 0 0] [1 0 2 1 0],
         [0 0 3 1 0] [0 0 3 2 0],
         [0 0 0 1 0] [0 0 0 2 0],
         [0 0 2 1 0] [0 0 2 2 0],
         [1 0 3 2 0] [1 0 3 3 0],
         [1 0 0 0 0] [1 0 0 1 0],
         [0 0 0 2 0] [0 0 0 3 0],
         [1 0 1 3 0] [1 0 2 0 0],
         [1 0 3 0 0] [1 0 3 1 0],
         [1 0 2 2 0] [1 0 2 3 0],
         [0 0 0 0 0] [0 0 0 1 0],
         [0 0 1 1 0] [0 0 1 2 0],
         [1 0 1 2 0] [1 0 1 3 0],
         [0 0 0 3 0] [0 0 1 0 0],
         [1 0 0 2 0] [1 0 0 3 0],
         [1 0 2 3 0] [1 0 3 0 0],
         [0 0 2 3 0] [0 0 3 0 0],
         [0 0 2 0 0] [0 0 2 1 0],
         [1 0 2 1 0] [1 0 2 2 0],
         [1 0 0 1 0] [1 0 0 2 0],
         [1 0 3 1 0] [1 0 3 2 0],
         [1 0 1 0 0] [1 0 1 1 0],
         [1 0 0 3 0] [1 0 1 0 0],
         [0 0 3 0 0] [0 0 3 1 0]},
        :index-backward-seq
        {[0 0 3 3 0] [0 0 3 2 0],
         [0 0 1 0 0] [0 0 0 3 0],
         [0 0 2 2 0] [0 0 2 1 0],
         [0 0 1 3 0] [0 0 1 2 0],
         [0 0 1 2 0] [0 0 1 1 0],
         [0 0 3 2 0] [0 0 3 1 0],
         [1 0 1 1 0] [1 0 1 0 0],
         [1 0 2 0 0] [1 0 1 3 0],
         [0 0 3 1 0] [0 0 3 0 0],
         [0 0 0 1 0] [0 0 0 0 0],
         [0 0 2 1 0] [0 0 2 0 0],
         [1 0 3 2 0] [1 0 3 1 0],
         [1 0 0 0 0] [0 0 3 3 0],
         [0 0 0 2 0] [0 0 0 1 0],
         [1 0 1 3 0] [1 0 1 2 0],
         [1 0 3 0 0] [1 0 2 3 0],
         [1 0 2 2 0] [1 0 2 1 0],
         [0 0 1 1 0] [0 0 1 0 0],
         [1 0 1 2 0] [1 0 1 1 0],
         [0 0 0 3 0] [0 0 0 2 0],
         [1 0 0 2 0] [1 0 0 1 0],
         [1 0 2 3 0] [1 0 2 2 0],
         [0 0 2 3 0] [0 0 2 2 0],
         [0 0 2 0 0] [0 0 1 3 0],
         [1 0 2 1 0] [1 0 2 0 0],
         [1 0 0 1 0] [1 0 0 0 0],
         [1 0 3 1 0] [1 0 3 0 0],
         [1 0 1 0 0] [1 0 0 3 0],
         [1 0 0 3 0] [1 0 0 2 0],
         [0 0 3 0 0] [0 0 2 3 0],
         [1 0 3 3 0] [1 0 3 2 0]}})
      (add-indexes init-comp)))

(def ^:private marks-in-order
  "sam/khaali labels in the order they are drawn across the avartan.
  This is the exact call views.cljs makes for each bhaag."
  db/bhaag-marks)

(deftest bhaag-start-beats-test
  (is (= [1 5 9 13] (db/bhaag-start-beats [4 4 4 4])))
  (is (= [1 3 6 8] (db/bhaag-start-beats [2 3 2 3])))
  (is (= [1 4 6] (db/bhaag-start-beats [3 2 2]))))

(deftest skip-khaali-numbering-test
  ;;khaali is not counted, so the bhaag after it is 3 and not 4
  (is (= ["x" "2" "o" "3"] (marks-in-order :teentaal false)))
  (is (= ["x" "2" "0" "3"] (marks-in-order :jhaptaal false))))

(deftest count-khaali-numbering-test
  ;;the sargam-spec numbering, kept for anyone taught that way
  (is (= ["x" "2" "o" "4"] (marks-in-order :teentaal true)))
  (is (= ["x" "2" "0" "4"] (marks-in-order :jhaptaal true))))

(deftest taals-already-skipping-khaali-are-unchanged-test
  ;;these taals are numbered without counting khaali upstream too,
  ;;so toggling must not move their labels
  (doseq [taal [:ektaal :rupak :dadra :kehrwa :adachautaal]]
    (is (= (marks-in-order taal true) (marks-in-order taal false))
        (str (name taal) " changed when it should not have"))))

(deftest sam-and-khaali-marks-preserved-test
  ;;every taal keeps one label per bhaag, and sam/khaali glyphs survive
  (doseq [taal (keys taal-def)]
    (doseq [count-khaali? [true false]]
      (let [marks (marks-in-order taal count-khaali?)]
        (is (= (count (:bhaags (taal-def taal))) (count marks))
            (str (name taal) " lost a bhaag label"))
        (is (= (filterv db/khaali-marks (marks-in-order taal true))
               (filterv db/khaali-marks marks))
            (str (name taal) " khaali markers moved"))))))
