(ns puppeteer.file-test
  (:require [cljs.test :refer-macros [deftest is testing async]]))

(deftest file-access-test
  (testing "Testing file access through Karma"
    (async done
      (js/console.log "Available files in /base directory:")
      (-> (js/fetch "/base/")
          (.then (fn [response]
                   (if (.-ok response)
                     (.text response)
                     (throw (js/Error. "Failed to fetch base directory listing")))))
          (.then (fn [text]
                   (js/console.log "Base directory contents:" text)
                   (is true "Successfully fetched base directory")
                   (done)))
          (.catch (fn [err]
                    (js/console.error "Failed to fetch base directory:" err)
                    (is false (str "Failed to fetch base directory: " (.-message err)))
                    (done)))))))
