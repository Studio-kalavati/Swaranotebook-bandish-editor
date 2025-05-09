(ns bhatkhande-editor.puppeteer-replay-test
  (:require [cljs.test :refer-macros [deftest is testing async]]
            [cljs.core.async :refer [go]]
            ["fs" :as fs]
            ["path" :as path]
            ["@puppeteer/replay" :refer [createRunner parse]]))

(deftest puppeteer-replay-test
  (async done
    (go
      (let [recordings-dir (path/join js/__dirname "../puppeteer_recordings")
            files (-> (fs/readdirSync recordings-dir)
                      (js->clj)
                      (->> (filter #(.endsWith % ".json"))))]
        
        (if (empty? files)
          (do
            (js/console.log "No replay JSON files found in recordings folder.")
            (done))
          
          (let [run-all-tests 
                (fn [files-to-run]
                  (if (empty? files-to-run)
                    (done)
                    (let [file (first files-to-run)
                          recording-path (path/join recordings-dir file)
                          recording-text (fs/readFileSync recording-path "utf8")
                          recording (parse (js/JSON.parse recording-text))]
                      
                      (js/console.log (str "Running replay for: " file))
                      
                      (-> (createRunner recording)
                          (.then (fn [runner]
                                   (-> (.run runner)
                                       (.then (fn []
                                                (js/console.log (str "Replay for " file " completed successfully."))
                                                (run-all-tests (rest files-to-run))))
                                       (.catch (fn [err]
                                                 (js/console.error (str "Replay for " file " failed:") err)
                                                 ;; Fail the test if any recording fails
                                                 (is false (str "Replay for " file " failed: " (.-message err)))
                                                 (done))))))
                          (.catch (fn [err]
                                    (js/console.error (str "Failed to create runner for " file ":") err)
                                    (is false (str "Failed to create runner for " file ": " (.-message err)))
                                    (done)))))))]
            
            (run-all-tests files)))))))
