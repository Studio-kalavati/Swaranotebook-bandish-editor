(ns bhatkhande-editor.utils
  (:require [clojure.string :as istr]
            [clojure.walk :refer [prewalk]]
            [clojure.spec.alpha :as s]))

;;svara is a 2-tuple [octave svara]
(s/def :snb/svara (s/and vector?
                         #(= 2 (count %))
                         #(#{:snb/mandra :snb/madhyam :snb/taar} (first %))
                         #(keyword? (second %))))

(s/valid? :snb/svara [:snb/mandra :snb/n])

(s/def :snb/notes
  (s/coll-of (s/keys :req [:snb/svara])))

(s/valid? :snb/notes [{:snb/svara [:snb/madhyam :snb/r]}])
(s/valid? :snb/notes [{:snb/svara [:snb/madhyam :snb/r]}])

(s/def :snb/noteseq (s/coll-of (s/keys :req [:snb/notes])))

(s/valid? :snb/noteseq [{:snb/notes [{:snb/svara [:snb/madhyam :snb/r]}]}
                        {:snb/notes [{:snb/svara [:snb/madhyam :snb/g]}]}])
(s/def :snb/part-title string?) ; or keyword? if you want tighter control

(s/def :snb/score-parts (s/coll-of (s/keys :req [:snb/part-title :snb/noteseq])))

(s/valid? :snb/score-parts [{:snb/part-title "sthayi"
                             :snb/noteseq [{:snb/notes [{:snb/svara [:snb/mandra :snb/n]}]}]}])

(s/def :snb/title string?)
(s/def :snb/taal keyword?) ;; e.g. :snb/jhaptaal
(s/def :snb/version string?)

(s/def :snb/composition
  (s/keys :req [:snb/title :snb/taal :snb/score-parts :snb/version]))


(s/valid? :snb/composition {:snb/title "Bandish"
                       :snb/taal :snb/jhaptaal
                       :snb/score-parts [{:snb/part-title "sthayi"
                                          :snb/noteseq [{:snb/notes [{:snb/svara [:snb/mandra :snb/n]}]}]}]
                       :snb/version "2025-25-01"})

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
                     (if (= :shruti x1) :svara x1)) x)) ijs))

(defn cvt2noteseq
  [kw-json]
  (let [k(strip-ns-keywords kw-json)]
    (println " k ")
    k)
  #_(let [{:keys [:snb/noteseq :snb/taal] :as imap} (get-in kw-json [:snb/score 0])]
      (when (and noteseq taal)
        (let [nns (strip-ns-keywords noteseq)
              j1 {:noteseq nns :taal (strip-ns-keywords taal)}]
          (println " json-onload " j1)
          j1))))

(defn json-onload
  [ijson]
  (let [kw-json (parse-json ijson)
        _ (println " parsed " kw-json)
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
