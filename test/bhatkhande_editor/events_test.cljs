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

(deftest move-cursor-forward-test
  (let [arg0 {:props {:cursor-pos {:row-index 0, :bhaag-index 1, :note-index 1, :nsi 0}}
              :composition {:index-forward-seq
                            {[0 0 0 0] [0 0 1 0], [0 0 1 0] [0 0 2 0], [0 0 2 0] [0 0 3 0], [0 0 3 0] [0 1 0 0], [0 1 0 0] [0 1 1 0], [0 1 1 0] [0 1 2 0]}}}
        res1 {:row-index 0, :bhaag-index 1, :note-index 2, :nsi 0}]
    (is (= res1
           (ev/move-cursor-forward arg0)))
    (is (= {:row-index 0, :bhaag-index 1, :note-index 3, :nsi 0}
           (ev/move-cursor-forward (-> (update-in arg0 [:composition :index-forward-seq]
                                                  assoc [0 1 2 0] [0 1 3 0])
                                       (update-in [:props :cursor-pos :note-index] inc)))))))


(deftest conj-svara-test
  (let [backward {[0 0 1 0] [0 0 0 0], [0 0 2 0] [0 0 1 0], [0 0 3 0] [0 0 2 0], [0 1 0 0] [0 0 3 0]}
        forward {[0 0 0 0] [0 0 1 0], [0 0 1 0] [0 0 2 0], [0 0 2 0] [0 0 3 0], [0 0 3 0] [0 1 0 0]}
        noteseq [{:notes [{:shruti [:madhyam :s], :npb 1}]}
                 {:notes [{:shruti [:madhyam :r], :npb 1}]}
                 {:notes [{:shruti [:madhyam :g], :npb 1}]}
                 {:notes [{:shruti [:madhyam :m], :npb 1}]}
                 ]
        arg0 {:props {:cursor-pos {:row-index 0, :bhaag-index 1, :note-index 0, :nsi 0}
                      :notes-per-beat 1}
              :composition {:index-forward-seq forward
                            :taal :teentaal
                            :noteseq noteseq
                            :index-backward-seq backward}}
        res {:db
             {:props
              {:cursor-pos {:row-index 0, :bhaag-index 1, :note-index 0, :nsi 0},
               :notes-per-beat 1},
              :composition
              {:index-forward-seq
               {[0 0 0 0] [0 0 1 0],
                [0 0 1 0] [0 0 2 0],
                [0 0 2 0] [0 0 3 0],
                [0 0 3 0] [0 1 0 0]},
               :taal :teentaal,
               :noteseq
               [{:notes [{:shruti [:madhyam :s], :npb 1}]}
                {:notes [{:shruti [:madhyam :r], :npb 1}]}
                {:notes [{:shruti [:madhyam :g], :npb 1}]}
                {:notes [{:shruti [:madhyam :m], :npb 1}]}
                {:notes [{:shruti [:madhyam :g], :npb 1}]}],
               :index-backward-seq
               {[0 0 1 0] [0 0 0 0],
                [0 0 2 0] [0 0 1 0],
                [0 0 3 0] [0 0 2 0],
                [0 1 0 0] [0 0 3 0]},
               :indexed-noteseq
               [[[{:notes [{:shruti [:madhyam :s], :npb 1}]}
                  {:notes [{:shruti [:madhyam :r], :npb 1}]}
                  {:notes [{:shruti [:madhyam :g], :npb 1}]}
                  {:notes [{:shruti [:madhyam :m], :npb 1}]}]
                 [{:notes [{:shruti [:madhyam :g], :npb 1}]}]]],
               :index [[0 0 0 0] [0 0 1 0] [0 0 2 0] [0 0 3 0] [0 1 0 0]]}},
             :dispatch [:bhatkhande-editor.events/save-to-localstorage]}
        ]
    (is (= res (ev/conj-svara {:db arg0} [nil {:svara {:shruti [:madhyam :g]}}])))
    )
  )


(deftest conj-sahitya-test
  (let [noteseq [{:notes [{:shruti [:madhyam :s], :npb 1}]}
                 {:notes [{:shruti [:madhyam :r], :npb 1}]}
                 {:notes [{:shruti [:madhyam :g], :npb 1}]}
                 {:notes [{:shruti [:madhyam :m], :npb 1}]}
                 ]
        arg0 {:composition {:taal :teentaal :noteseq noteseq}}
        res {:db
             {:composition
              {:taal :teentaal,
               :noteseq
               [{:notes [{:shruti [:madhyam :s], :npb 1}], :lyrics "abcd"}
                {:notes [{:shruti [:madhyam :r], :npb 1}]}
                {:notes [{:shruti [:madhyam :g], :npb 1}]}
                {:notes [{:shruti [:madhyam :m], :npb 1}]}]}}}
        ]
    (is (= res (ev/conj-sahitya {:db arg0} [nil {:text-val "abcd" :bhaag-index 0 :row-index 0}])))
    )
  )

(deftest next-bhaag-lyrics-popup-test
  (let [backward {[0 0 1 0] [0 0 0 0], [0 0 2 0] [0 0 1 0], [0 0 3 0] [0 0 2 0], [0 1 0 0] [0 0 3 0]}
        forward {[0 0 0 0] [0 0 1 0], [0 0 1 0] [0 0 2 0], [0 0 2 0] [0 0 3 0], [0 0 3 0] [0 1 0 0]}
        arg0 {:props {}
              :composition {:index-forward-seq forward
                            :index-backward-seq backward}}
        res {:row-index 0, :bhaag-index 1}]
    (-> (ev/next-bhaag-lyrics-popup {:db arg0} [nil {:bhaag-index 0 :row-index 0}])
        :db
        :props
        :show-lyrics-popup
        (= res)
        is
        )
    ))

(deftest update-highlight-pos-test
  (let [backward {[0 0 1 0] [0 0 0 0], [0 0 2 0] [0 0 1 0], [0 0 3 0] [0 0 2 0], [0 1 0 0] [0 0 3 0]}
        forward {[0 0 0 0] [0 0 1 0], [0 0 1 0] [0 0 2 0], [0 0 2 0] [0 0 3 0], [0 0 3 0] [0 1 0 0]}
        arg0 {:props {:cursor-pos {:row-index 0, :bhaag-index 0, :note-index 2, :nsi 0}}
              :composition {:index-forward-seq forward
                            :taal :teentaal
                            :index-backward-seq backward}}
        res {:cursor-pos {:row-index 0, :bhaag-index 0, :note-index 3, :nsi 0}, :highlighted-pos '({:row-index 0, :bhaag-index 0, :note-index 2, :nsi 0})}
        res-left {:cursor-pos {:row-index 0, :bhaag-index 0, :note-index 1, :nsi 0},
                  :highlighted-pos [{:row-index 0, :bhaag-index 0, :note-index 1, :nsi 0}]}]
    (-> (ev/update-highlight-pos {:db arg0} [nil :right])
        :db :props (= res) is)
    (-> (ev/update-highlight-pos {:db arg0} [nil :left])
        :db :props (= res-left) is)))
