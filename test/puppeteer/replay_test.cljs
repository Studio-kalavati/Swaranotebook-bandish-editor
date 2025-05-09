(ns puppeteer.replay-test
  (:require [cljs.test :refer-macros [deftest is testing async]]
            [cljs.core.async :refer [go]]
            ["@puppeteer/replay" :refer [createRunner parse]]))

(deftest replay-single-recording-test
  (testing "Replaying a single recording from puppeteer_recordings"
    (async done
      (let [recording-file "recording1.json"
            recording-url (str "/base/puppeteer_recordings/" recording-file)]
        (-> (js/fetch recording-url)
            (.then (fn [response]
                     (if (.-ok response)
                       (.json response)
                       (throw (js/Error. (str "Failed to fetch recording: " recording-file))))))
            (.then (fn [recording-json]
                     (let [recording (parse recording-json)]
                       (js/console.log (str "Running replay for: " recording-file))
                       (-> (createRunner recording)
                           (.then (fn [runner]
                                    (-> (.run ^js runner)
                                        (.then (fn []
                                                 (js/console.log (str "Replay for " recording-file " completed successfully."))
                                                 (is true "Replay completed successfully")
                                                 (done)))
                                        (.catch (fn [err]
                                                  (js/console.error (str "Replay for " recording-file " failed:") err)
                                                  (is false (str "Replay for " recording-file " failed: " (.-message err)))
                                                  (done))))))
                           (.catch (fn [err]
                                     (js/console.error (str "Failed to create runner for " recording-file ":") err)
                                     (is false (str "Failed to create runner for " recording-file ": " (.-message err)))
                                     (done)))))))
            (.catch (fn [err]
                      (js/console.error (str "Failed to fetch recording " recording-file ":") err)
                      (is false (str "Failed to fetch recording " recording-file ": " (.-message err)))
                      (done))))))))
