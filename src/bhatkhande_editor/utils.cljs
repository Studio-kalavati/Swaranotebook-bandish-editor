(ns bhatkhande-editor.utils
  (:require [clojure.string :as istr]
            [clojure.walk :refer [prewalk]]
            [clojure.spec.alpha :as s]))

(s/def :snb/taal keyword?)

(s/def :snb/part-id string?)
(s/def :snb/noteseq vector?)
(s/def :snb/part (s/keys :req [:snb/taal :snb/noteseq :snb/part-id]))
(s/def :snb/score (s/coll-of :snb/part))
(s/def :snb/composition (s/keys :req [:snb/score]))

(def key-fn #(keyword "snb" %))

(defn prewalk-fn
  [x]
  (do
      (cond
        (map-entry? x)
        (let [ikey (key x)]
          (cond
            (string? ikey) {(key-fn ikey)
                            (if (= ikey "taal") (key-fn (val x))
                                (val x))}
            :else x))
        (and (vector? x) ( = 2 (count x))) (mapv key-fn x)
        :else x)))

(defn parse-json
  [json-str]
  (prewalk prewalk-fn json-str))

(defn strip-ns-keywords
  [ijs]
  (prewalk (fn[x](if (keyword? x)
                   (let [x1 (keyword (first (istr/split (name x)"/" )))]
                     (if (= :svara x1) :shruti x1)) x)) ijs))

(defn cvt2noteseq
  [kw-json]
  (let [{:keys [:snb/noteseq :snb/taal] :as imap} (get-in kw-json [:snb/score 0])]
    (when (and noteseq taal)
      (let [nns (strip-ns-keywords noteseq)
            j1 {:noteseq nns :taal (strip-ns-keywords taal)}]
        (println " json-onload " j1)
        j1))))

(defn json-onload
  [ijson]
  (let [kw-json (parse-json ijson)
        isvalid? (s/valid? :snb/composition kw-json)]
    (if-not isvalid? (println " ex " (s/explain :snb/composition kw-json)))
    (cvt2noteseq kw-json)))

(defn cursor2vec
  "return a cursor array like [0 1 0 1 0], given a map argument like so:
  {:score-part-index 0, :avartan-index 1, :bhaag-index 0, :note-index 2, :nsi 0}"
  [cursor]
  (mapv #(% cursor)
        [:score-part-index :avartan-index :bhaag-index :note-index :nsi]))

(defn cursor2map
  "return a cursor map like {:score-part-index 0, :row-index 1, :bhaag-index 0, :note-index 2, :nsi 0}, given a vector argument like so [0 1 0 1 0]"
  [cursor-vec]
  (zipmap [:score-part-index :avartan-index :bhaag-index :note-index :nsi] cursor-vec))

(defn get-noteseq-key
  "return the key for a noteseq, by fetching the score-part-index from the cursor"
  [db]
  (let [cursor-pos (get-in db [:props :cursor-pos])
        score-part-index (:score-part-index cursor-pos)]
    [:composition :score-parts score-part-index :noteseq]))

(defn remove-empty-avartan
  "remove empty avartan from the end of a flat noteseq"
  [flat-noteseq num-beats]
  (let [empty-notes (->> flat-noteseq reverse
                         (take-while #(= % {:notes [{:svara [:madhyam :_]}]})))]
    ;;remove it if more than last avartan plus 1 beat is empty, with no lyrics written
    (if (>= (count empty-notes) (inc num-beats) )
      (subvec flat-noteseq 0 (- (count flat-noteseq) num-beats))
      flat-noteseq))
  )
