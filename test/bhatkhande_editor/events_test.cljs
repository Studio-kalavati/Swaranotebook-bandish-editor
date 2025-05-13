(ns bhatkhande-editor.events-test
  (:require [cljs.test :refer (deftest is)]
            [bhatkhande-editor.utils :as ut]
            [bhatkhande-editor.events :as ev]
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

(deftest keyboard-conj
  (let [svara [:madhyam :m]]
    (is
     (= (let [db {:props {:notes-per-beat 1
                          :note-octave :madhyam}}
              res (ev/keyboard-conj-svara {:db db} [:a :m])]
          (:dispatch-n res))
        [[:bhatkhande-editor.events/conj-svara {:svara {:shruti svara}}]
         [:bhatkhande-editor.events/play-svara [:madhyam :m]]]))))

(deftest insert-start-note
  (let [nsvara {:shruti [:madhyam :r], :npb 1}]
    (is (= (ev/insert-note{:note-index -1, :nsvara nsvara} [])
           [{:notes [nsvara]}]))
    (is (= (ev/insert-note {:note-index 0, :nsvara nsvara} [0])
           [0 {:notes [nsvara]}]))
    (is (= (ev/insert-note {:note-index 1, :nsvara nsvara}[0 1])
           [0 1 {:notes [nsvara]}]))
    ))

(deftest insert-next-note-test
  ;;add 1 note per beat
  (is (=
       [[{:notes [{:shruti [:madhyam :r], :npb 1}]}] :next-note-cursor])
      (ev/update-noteseq
       {:note-index -1,
        :svara {:shruti [:madhyam :r]},
        :notes-per-beat 1,
        :cpos {:row-index 0, :bhaag-index 0, :note-index 0, :nsi 0},}
       []))
  (is (=
       [[{:notes [{:shruti [:madhyam :r], :npb 1}]}
         {:notes [{:shruti [:madhyam :g], :npb 1}]}]
        :next-note-cursor])
      (ev/update-noteseq
       {:note-index 0,
        :svara {:shruti [:madhyam :g], :npb 1},
        :notes-per-beat 1,
        :cpos {:row-index 0, :bhaag-index 0, :note-index 0, :nsi 0},}
       [{:notes [{:shruti [:madhyam :r], :npb 1}]}]))
  ;;add 2 notes per beat
  (let [arg0 {:note-index 0,
              :svara {:shruti [:madhyam :g]},
              :notes-per-beat 2,
              :cpos {:row-index 0, :bhaag-index 0, :note-index 1, :nsi 0},}
        noteseq [{:notes [{:shruti [:madhyam :r], :npb 1}]}]]
    (is
     (let [resp (ev/update-noteseq arg0 noteseq)]
       (= (first resp)
          [{:notes [{:shruti [:madhyam :r], :npb 1}]}
           {:notes [{:shruti [:madhyam :g], :npb 2}]}])))
    ;;add the second note per 2 beats
    (is
     (let [resp (ev/update-noteseq arg0 noteseq)
           resp2 (ev/update-noteseq (-> (update-in arg0 [:cpos :nsi] inc)
                                        (update-in [:note-index] inc)) (first resp))]
       (= (first resp2)
          [{:notes [{:shruti [:madhyam :r], :npb 1}]}
           {:notes [{:shruti [:madhyam :g], :npb 2} {:shruti [:madhyam :g], :npb 2}]}])))))

