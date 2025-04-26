(ns bhatkhande-editor.events-test
  (:require [cljs.test :refer (deftest is)]
            [bhatkhande-editor.utils :as ut]
            [clojure.spec.alpha :as s]
            ))

(def score {:snb/score [{:snb/part-id "A" :snb/noteseq
                         [{:snb/notes [{:snb/svara [:snb/mandra :snb/s]}],
                           :snb/lyrics "Oo,Darlin',I've,been"}],
                         :snb/taal :snb/teentaal}]})

(deftest valid-score?
  (is (s/valid? :snb/composition score)))

(deftest json-2-ns
  (is
   (= score
    (ut/parse-json {"score" [{"part-id" "A", "noteseq"
                              [{"notes" [{"svara" ["mandra" "s"]}],
                                "lyrics" "Oo,Darlin',I've,been"}]
                              "taal" "teentaal"}]}))))

(deftest nsjson-to-noteseq
  (is
   (= {:noteseq [{:notes [{:shruti [:mandra :s]}], :lyrics "Oo,Darlin',I've,been"}], :taal :teentaal}
      (ut/cvt2noteseq (ut/parse-json {"score" [{"part-id" "A", "noteseq"
                                                [{"notes" [{"svara" ["mandra" "s"]}],
                                                  "lyrics" "Oo,Darlin',I've,been"}]
                                                "taal" "teentaal"}]})))))
