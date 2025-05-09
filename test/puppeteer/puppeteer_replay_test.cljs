(ns puppeteer.puppeteer-replay-test
  (:require [cljs.test :refer-macros [deftest is testing async]]
            [cljs.core.async :refer [go]]
            ["@puppeteer/replay" :refer [createRunner parse]]))

(deftest puppeteer-replay-test
  (async done
    (go
      ;; In browser environment, we need to fetch recordings from a URL instead of filesystem
      (let [recordings-url "/base/puppeteer_recordings"  ;; Karma serves files from project root
            fetch-recordings (fn []
                               (-> (js/fetch (str recordings-url "/index.json"))
                                   (.then (fn [response]
                                            (if (.-ok response)
                                              (.json response)
                                              (throw (js/Error. "Failed to fetch recordings index")))))
                                   (.then (fn [files-json]
                                            (js->clj files-json)))))]
        
        (-> (fetch-recordings)
            (.then (fn [files]
                     (if (empty? files)
                       (do
                         (js/console.log "No replay JSON files found in recordings index.")
                         (done))
                       (let [run-all-tests (fn run-all-tests-fn [files-to-run]
                                             (if (empty? files-to-run)
                                               (done)
                                               (let [file (first files-to-run)]
                                                 (-> (js/fetch (str recordings-url "/" file))
                                                     (.then (fn [response]
                                                              (if (.-ok response)
                                                                (.json response)
                                                                (throw (js/Error. (str "Failed to fetch recording: " file))))))
                                                     (.then (fn [recording-json]
                                                              (let [recording (parse recording-json)]
                                                                (js/console.log (str "Running replay for: " file))
                                                                (-> (createRunner recording)
                                                                    (.then (fn [runner]
                                                                             (-> (.run ^js runner)
                                                                                 (.then (fn []
                                                                                          (js/console.log (str "Replay for " file " completed successfully."))
                                                                                          (run-all-tests-fn (rest files-to-run))))
                                                                                 (.catch (fn [err]
                                                                                           (js/console.error (str "Replay for " file " failed:") err)
                                                                                           (is false (str "Replay for " file " failed: " (.-message err)))
                                                                                           (done))))))
                                                                    (.catch (fn [err]
                                                                              (js/console.error (str "Failed to create runner for " file ":") err)
                                                                              (is false (str "Failed to create runner for " file ": " (.-message err)))
                                                                              (done)))))))
                                                     (.catch (fn [err]
                                                               (js/console.error (str "Failed to fetch recording " file ":") err)
                                                               (is false (str "Failed to fetch recording " file ": " (.-message err)))
                                                               (done))))))))]
                         (run-all-tests files)))))
            (.catch (fn [err]
                      (js/console.error "Failed to fetch recordings index:" err)
                      (is false (str "Failed to fetch recordings index: " (.-message err)))
                      (done))))))))
