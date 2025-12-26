(ns bhatkhande-editor.utils
  (:require [clojure.string :as istr]
            [clojure.walk :refer [prewalk]]
            [clojure.spec.alpha :as s]))

;;svara is a 2-tuple [octave svara]
(s/def :snb/svara (s/and vector?
                         #(= 2 (count %))
                         #(#{:snb/mandra :snb/madhyam :snb/taar} (first %))
                         #(keyword? (second %))))

(s/def :snb/lyrics string?)
(s/valid? :snb/svara [:snb/mandra :snb/n])

(s/def :snb/notes
  (s/coll-of (s/keys :req [:snb/svara] :opt [:snb/lyrics])))

(s/valid? :snb/notes [{:snb/svara [:snb/madhyam :snb/r]}])
(s/valid? :snb/notes [{:snb/svara [:snb/madhyam :snb/r]}])

(s/def :snb/noteseq (s/coll-of (s/keys :req [:snb/notes])))

(s/valid? :snb/noteseq [{:snb/notes [{:snb/svara [:snb/madhyam :snb/r]}]}
                        {:snb/notes [{:snb/svara [:snb/madhyam :snb/g]}]}])

(s/valid? :snb/noteseq [{:snb/notes [{:snb/svara [:snb/madhyam :snb/r]}] :lyrics "a"}
                        {:snb/notes [{:snb/svara [:snb/madhyam :snb/g]}] :lyrics "b"}])

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
                                          :snb/noteseq [{:snb/notes [{:snb/svara [:snb/mandra :snb/n] :lyrics "a"}]}]}]
                       :snb/version "2025-25-01"})

(def key-fn #(keyword "snb" %))

(defn transform-entry
  "walk the json input and convert most keys to namespaced keywords.
  For some values, keep it as a string (e.g. lyrics, title),
  and or the rest, convert values to namespaced keywords too"
  [x]
  (let [ns-prefix :snb]
    (cond
      (map? x)
      (into {}
            (for [[k v] x]
              (let [new-k (keyword (name ns-prefix) (name k))
                    new-v (cond
                            ;;for these keys, don't convert the value into a keyword, leave it as a string
                            (and (#{"lyrics" "title" "part-title" "version"} k) (string? v))
                            v
                            (and (= k "svara") (vector? v))
                            (mapv #(keyword (name ns-prefix) (name %)) v)
                            :else (if (or (keyword? v) (string? v))
                                    (keyword (name ns-prefix) (name v))
                                    v))]
                [new-k new-v])))
      :else x)))

(defn parse-json
  [json-str]
  (prewalk transform-entry json-str))

(defn strip-ns-keywords
  [ijs]
  (prewalk (fn[x](if (keyword? x)
                   (let [x1 (keyword (first (istr/split (name x)"/" )))]
                     (if (= :shruti x1) :svara x1)) x)) ijs))

(defn json-onload
  [ijson]
  (let [kw-json (parse-json ijson)
        isvalid? (s/valid? :snb/composition kw-json)]
    (if isvalid?
      (strip-ns-keywords kw-json)
      (do
        (s/explain :snb/composition kw-json)
        (throw (js/Error. (str "Invalid JSON format: "
                               (with-out-str (s/explain :snb/composition kw-json)))))))))

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
     (if (>= (count empty-notes) (inc num-beats))
       (subvec flat-noteseq 0 (- (count flat-noteseq) num-beats))
       flat-noteseq)))

(defn extract-youtube-video-id
  "Extract YouTube video ID from various URL formats.
   Supports:
   - https://www.youtube.com/watch?v=VIDEO_ID
   - https://youtu.be/VIDEO_ID
   - https://www.youtube.com/embed/VIDEO_ID
   - https://m.youtube.com/watch?v=VIDEO_ID
   Returns nil if no valid video ID is found."
  [url]
  (when url
    (cond
      (istr/includes? url "youtube.com/watch")
      (let [match (re-find #"[?&]v=([^&]+)" url)]
        (second match))

      (istr/includes? url "youtu.be/")
      (let [match (re-find #"(?:youtu\.be/|youtube\.com/embed/)([^\?&]+)" url)]
        (second match))

      (istr/includes? url "youtube.com/embed/")
      (let [match (re-find #"embed/([^\?&]+)" url)]
        (second match))

      :else nil)))
