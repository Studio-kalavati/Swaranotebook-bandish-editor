(ns bandish-community.core
  (:require [net.cgrand.enlive-html :as html]
            [clojure.java.io :as io]
            [hiccup.core :as h]
            [clojure.xml :as xml]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.zip :as zip])
  (:gen-class))

(def root "resources/public/")
(def page-path (str root "pages/"))

(defn get-full-url
  [filepath]
  (clojure.string/replace filepath root "https://bandish.netlify.app/"))

(defn format-title
  [title]
  (-> (.toLowerCase title)
      (clojure.string/replace  " " "_")
      (clojure.string/replace  "/" "_")
      (str  ".html")))

(defn isong
  [{:keys [iurl song-title description]}]
  [:div
   [:a {:href (str iurl "&mode=play") } song-title]
   [:p description]])

(html/deftemplate list-template "pagetemplates/listtemplate.html"
  [songs]
  [:body]
  (->> (mapv isong songs)
       (into [:div {:class "par"}])
       (html/html) (html/content)))

(defn generate-list
  [{:keys [basepath song-list]}]
  (let []
    (spit basepath (apply str
                          (list-template
                           (sort-by :song-title song-list))))
    basepath))

(comment
  (def data1
    [{:iurl "https://bandish.netlify.app/view/vlB9hVgb5OdKcadbhOOYyzKwvpl2/34e847c547ba-aasu-bharihai"
      :description "abcd"
      :song-title "Ye aasu Bhari Hai"}])
  (generate-list {:basepath "resources/public/index.html"
                  :song-list data1}))

(defn isite
  [timestr href]
  (let []
    (into [:url]
          [[:loc href]
           [:lastmod timestr]])))

(defn sitemap-list
  [basepath song-list]
  (let [timestr (.format (new java.text.SimpleDateFormat "yyyy-MM-dd")
                         (new java.util.Date))
        songs (->> song-list
                   (map :iurl)
                   (mapv (partial isite timestr)))]
    (spit (str basepath "sitemap.xml" )
          (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
               (->> songs
                    (into
                     [:urlset
                      {:xmlns
                       "http://www.sitemaps.org/schemas/sitemap/0.9"
                       :xmlns:xhtml "http://www.w3.org/1999/xhtml"
                       :xmlns:image
                       "http://www.google.com/schemas/sitemap-image/1.1"}])
                    (h/html))))))

;;(sitemap-list "resources/public/" data1)


(defn generate-all-pages
  []
  (let [song-list (:songs (with-open [r (io/reader "doc/curatedsongs.edn")]
                           (edn/read (java.io.PushbackReader. r))))
        songlisttext "resources/public/index.html"
        text-index (generate-list
                    {:basepath songlisttext
                     :song-list song-list})]
    (sitemap-list "resources/public/" song-list)))

(defn zip-str [s]
  (let [istr (slurp s)]
    (zip/xml-zip 
     (xml/parse (java.io.ByteArrayInputStream. (.getBytes istr))))))

(defn get-links
  []
  (->> (zip-str "resources/public/sitemap.xml")
       (iterate zip/next)
       (take-while #(not (zip/end? %)))
       (map zip/node)
       rest
       (map (comp :content first :content))
       (filter identity)
       (map first)))
;;(def k1 (zip-str "resources/public/sitemap.xml"))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println " run main ")
  (generate-all-pages))
