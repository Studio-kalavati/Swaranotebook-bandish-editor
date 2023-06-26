(ns bhatkhande-editor.sitemap
  (:require [adzerk.env :as env]
            [hiccup.core :as h])
  (:import
   [com.google.api.gax.paging Page]
   [com.google.cloud.storage Blob Storage StorageOptions
    Storage$BlobListOption]))

(env/def
  apiKey :required
  authDomain :required
  projectId :required
  storageBucket :required
  messagingSenderId :required
  appId :required
  siteUrl :required
  posthogKey :required)

(defn isite
  [timestr song-path]
  (let [href (str siteUrl "/view/" song-path)]
    (into [:url]
          (into
           [[:loc href]
            [:lastmod timestr]]))))

(defn sitemap-list
  [basepath song-list]
  (let [timestr (.format (new java.text.SimpleDateFormat "yyyy-MM-dd")
                         (new java.util.Date))
        songs (->> song-list
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

(defn spit-sitemap
  []
  (let [svc (.getService (.build (.setProjectId (StorageOptions/newBuilder) projectId)))
        ilist (.list svc storageBucket
               (into-array Storage$BlobListOption []))]
    (sitemap-list "resources/public/"
                  (mapv #(.getName %) (.iterateAll ilist)))))
