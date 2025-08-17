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
