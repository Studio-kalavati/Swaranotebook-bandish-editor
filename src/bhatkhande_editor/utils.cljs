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
