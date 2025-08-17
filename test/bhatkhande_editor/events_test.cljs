(ns bhatkhande-editor.events-test
  (:require [cljs.test :refer (deftest is)]
            [bhatkhande-editor.utils :as ut]
            [bhatkhande-editor.events :as ev]
            [bhatkhande-editor.db :as db]
            [clojure.spec.alpha :as s]))

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
   (= {:noteseq [{:notes [{:shruti [:mandra :s]}], :lyrics "a"}], :taal :teentaal}
      (ut/cvt2noteseq (ut/parse-json {"score" [{"part-id" "A", "noteseq"
                                                [{"notes" [{"svara" ["mandra" "s"]}],
                                                  "lyrics" "a"}]
                                                "taal" "teentaal"}]})))
   (= {:noteseq [{:notes [{:shruti [:mandra :s]}], :lyrics "a"}
                 {:notes [{:shruti [:mandra :s]}], :lyrics "b"}], :taal :teentaal}
      (ut/cvt2noteseq (ut/parse-json {"score" [{"part-id" "A", "noteseq"
                                                [{"notes" [{"svara" ["mandra" "s"]}],
                                                  "lyrics" "a"},
                                                 {"notes" [{"svara" ["mandra" "s"]}],
                                                  "lyrics" "b"}]
                                                "taal" "teentaal"}]})))))

(deftest keyboard-conj
  (let [svara [:madhyam :m]]
    (is
     (= (let [db {:props {:notes-per-beat 1
                          :note-octave :madhyam}}
              res (ev/keyboard-conj-svara {:db db}[:a :m])]
          (:dispatch-n res))
        [[:bhatkhande-editor.events/conj-svara {:svara {:svara svara}}]
         [:bhatkhande-editor.events/play-svara [:madhyam :m]]]))))

(deftest insert-next-note-test
  ;;add 1 note per beat
  (let [indexed-noteseq
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
            {:notes [{:svara [:madhyam :_]}]}]]]]]
    (is (=
         [[{:notes [{:svara [:madhyam :r]}], :lyrics "a"}
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
           {:notes [{:svara [:madhyam :_]}]}]
          :next-note-cursor]
         (ev/update-noteseq
          {:svara {:svara [:madhyam :r]},
           :taal :teentaal
           :notes-per-beat 1,
           :cpos {:score-part-index 0 :avartan-index 0 :bhaag-index 0 :note-index 0 :nsi 0}}
          ;;replace the first note with :r
          indexed-noteseq)))))

(deftest delete-single-svara-test
  (is
   (=
    [{:notes [{:svara [:madhyam :s]}], :lyrics "a"}
     {:notes [{:svara [:madhyam :r]}], :lyrics "b"}
     {:notes [{:svara [:madhyam :g]}], :lyrics "c"}
     {:notes [{:svara [:madhyam :m]}], :lyrics "d"}
     {:notes [{:svara [:madhyam :p]}], :lyrics "a"}
     {:notes [{:svara [:madhyam :d]}], :lyrics "b"}
     {:notes [{:svara [:madhyam :_]}], :lyrics "c"}
     {:notes [{:svara [:madhyam :_]}], :lyrics "d"}
     {:notes [{:svara [:madhyam :_]}]}
     {:notes [{:svara [:madhyam :_]}]}
     {:notes [{:svara [:madhyam :_]}]}
     {:notes [{:svara [:madhyam :_]}]}
     {:notes [{:svara [:madhyam :_]}]}
     {:notes [{:svara [:madhyam :_]}]}
     {:notes [{:svara [:madhyam :_]}]}
     {:notes [{:svara [:madhyam :_]}]}]
    (let [idb {:db (db/comp-decorator db/init-comp)}]
      (-> idb
          (ev/delete-single-svara [])
          (ev/delete-single-svara [])
          (get-in [:db :composition :score-parts 0 :noteseq ])))))
  (is (=
       [{:notes [{:svara [:madhyam :s]}], :lyrics "a"}
        {:notes [{:svara [:madhyam :r]}], :lyrics "b"}
        {:notes [{:svara [:madhyam :g]}], :lyrics "c"}
        {:notes [{:svara [:madhyam :m]}], :lyrics "d"}
        {:notes [{:svara [:madhyam :p]}], :lyrics "a"}
        {:notes [{:svara [:madhyam :d]}], :lyrics "b"}
        {:notes [{:svara [:madhyam :n]}], :lyrics "c"}
        {:notes [{:svara [:madhyam :_]}], :lyrics "d"}
        {:notes [{:svara [:madhyam :_]}]}
        {:notes [{:svara [:madhyam :_]}]}
        {:notes [{:svara [:madhyam :_]}]}
        {:notes [{:svara [:madhyam :_]}]}
        {:notes [{:svara [:madhyam :_]}]}
        {:notes [{:svara [:madhyam :_]}]}
        {:notes [{:svara [:madhyam :_]}]}
        {:notes [{:svara [:madhyam :_]}]}]
       (->
        (ev/delete-single-svara
         {:db (db/comp-decorator db/init-comp)}
         [])
        (get-in [:db :composition :score-parts 0 :noteseq])))))

(deftest conj-svara-test
  (is (= {:notes [{:svara [:madhyam :s]}]}
         (->
          (ev/conj-svara
           {:db (db/comp-decorator db/init-comp)}
           [nil {:svara {:svara [:madhyam :s]}}])
          (get-in [:db :composition :score-parts 0 :noteseq 8]))))
  ;;2 notes per beat
  (is (= {:notes [{:svara [:madhyam :s]} {:svara [:madhyam :_]}]}
         (->
          (ev/conj-svara
           {:db (-> (db/comp-decorator db/init-comp)
                    (update-in [:props :notes-per-beat] (constantly 2)))}
           [nil {:svara {:svara [:madhyam :s]}}])
          (get-in [:db :composition :score-parts 0 :noteseq 8]))))
  ;;add 2 notes multiple times
  (is
   (=
    ;;add 3 notes, the first 2 are in 1 beat
    ;;in the seccond beat, the second note is empty
    [{:notes [{:svara [:madhyam :s]} {:svara [:madhyam :s]}]}
     {:notes [{:svara [:madhyam :s]} {:svara [:madhyam :_]}]}]
    (let [idb {:db (-> (db/comp-decorator db/init-comp)
                       (update-in [:props :notes-per-beat] (constantly 2)))}]
      (->
       idb
       (ev/conj-svara [nil {:svara {:svara [:madhyam :s]}}])
       (ev/conj-svara [nil {:svara {:svara [:madhyam :s]}}])
       (ev/conj-svara [nil {:svara {:svara [:madhyam :s]}}])
       (get-in [:db :composition :score-parts 0 :noteseq])
       (subvec 8 10))))))

(deftest cursor-forward-test
  (let [idb (db/comp-decorator db/init-comp)]
    (is
     (=
      {:score-part-index 0, :avartan-index 0, :bhaag-index 2, :note-index 1, :nsi 0}
      (ev/move-cursor-forward idb)))
    (is
     (=
      ;;on  deleting 2 notes, cursor should move to the last note in the previous bhaag
      {:score-part-index 0, :avartan-index 0, :bhaag-index 1, :note-index 3, :nsi 0}
      (-> {:db idb}
          (ev/delete-single-svara [])
          (ev/delete-single-svara [])
          :db
          (ev/move-cursor-forward))))))

(deftest conj-sahitya-test
  (let [idb (db/comp-decorator db/init-comp)]
    (is (=
         '("x" "x" "x" "x")
         (-> {:db idb}
             (ev/conj-sahitya
              [nil {:text-val (vec (repeat 4 "x")) :score-part-index 0 :avartan-index 0 :bhaag-index 0}])
             :db
             (get-in [:composition :score-parts 0 :noteseq])
             (as-> ivec (->> ivec (take 4) (map :lyrics))))))))

#_(deftest next-bhaag-lyrics-popup-test
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

#_(deftest update-highlight-pos-test
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

(deftest get-play-at-time-seq-test
  (is
   (=
    '([:tick 5 0.5]
      [[:madhyam :s] 5 0.5]
      [[:madhyam :r] 5.5 0.5]
      [[:madhyam :g] 6 0.5]
      [[:madhyam :m] 6.5 0.5]
      [:tick 7 0.5]
      [[:madhyam :p] 7 0.5]
      [[:madhyam :d] 7.5 0.5]
      [[:madhyam :n] 8 0.5]
      [[:taar :s] 8.5 0.5]
      [:tick 9 0.5]
      [[:madhyam :_] 9 0.5]
      [[:madhyam :_] 9.5 0.5]
      [[:madhyam :_] 10 0.5]
      [[:madhyam :_] 10.5 0.5]
      [:tick 11 0.5]
      [[:madhyam :_] 11 0.5]
      [[:madhyam :_] 11.5 0.5]
      [[:madhyam :_] 12 0.5]
      [[:madhyam :_] 12.5 0.5]
      [:tick 13 0.5]
      [[:taar :s] 13 0.5]
      [[:madhyam :n] 13.5 0.5]
      [[:madhyam :d] 14 0.5]
      [[:madhyam :p] 14.5 0.5]
      [:tick 15 0.5]
      [[:madhyam :m] 15 0.5]
      [[:madhyam :g] 15.5 0.5]
      [[:madhyam :r] 16 0.5]
      [[:madhyam :s] 16.5 0.5]
      [:tick 17 0.5]
      [[:madhyam :_] 17 0.5]
      [[:madhyam :_] 17.5 0.5]
      [[:madhyam :_] 18 0.5]
      [[:madhyam :_] 18.5 0.5]
      [:tick 19 0.5]
      [[:madhyam :_] 19 0.5]
      [[:madhyam :_] 19.5 0.5]
      [[:madhyam :_] 20 0.5]
      [[:madhyam :_] 20.5 0.5])
    (let [idb {:db (-> (db/comp-decorator db/init-comp)
                       #_(update-in [:props :bpm] (constantly 60)))}]
      (ev/get-play-at-time-seq
       {:composition (-> idb :db :composition),
        :beat-mode :metronome, :bpm 120, :play-head-position 0, :now 5.0})))))
