(ns bhatkhande-editor.puppeteer.basic-test
  (:require [cljs.test :refer-macros [deftest is testing async]]
            [cljs.core.async :refer [go timeout <!]]
            ["puppeteer" :as puppeteer]))

(deftest basic-page-load-test
  (async done
    (go
      (let [browser (await (.launch puppeteer #js {:headless true}))
            page (await (.newPage browser))
            url (str "http://localhost:" (.-SERVER_PORT js/goog.global) "/index.html")]
        
        (try
          (await (.goto page url #js {:waitUntil "networkidle0"}))
          (is (= "Bhatkhande Editor" (await (.title page))))
          
          (finally
            (await (.close browser))
            (done)))))))
