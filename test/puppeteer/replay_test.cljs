(ns puppeteer.replay-test
  (:require [cljs.test :refer-macros [deftest is testing async]]
            [cljs.core.async :refer [go]]))

;; Simple test that loads a recording JSON file
(deftest load-recording-test
  (testing "Loading a recording from puppeteer_recordings"
    (async done
      (let [recording-file "recording1.json"
            recording-url (str "/base/puppeteer_recordings/" recording-file)]
        (-> (js/fetch recording-url)
            (.then (fn [response]
                     (if (.-ok response)
                       (.json response)
                       (throw (js/Error. (str "Failed to fetch recording: " recording-file))))))
            (.then (fn [recording-json]
                     (js/console.log (str "Successfully loaded recording: " recording-file))
                     (is (map? (js->clj recording-json)) "Recording should be a valid JSON object")
                     (is (contains? (js->clj recording-json) "steps") "Recording should contain steps")
                     (done)))
            (.catch (fn [err]
                      (js/console.error (str "Failed to fetch recording " recording-file ":") err)
                      (is false (str "Failed to fetch recording " recording-file ": " (.-message err)))
                      (done))))))))

;; Mock implementation of a simple replay test
;; This doesn't use the problematic @puppeteer/replay package
(deftest mock-replay-test
  (testing "Mock replay of a recording"
    (async done
      (let [recording-file "recording1.json"
            recording-url (str "/base/puppeteer_recordings/" recording-file)]
        (-> (js/fetch recording-url)
            (.then (fn [response]
                     (if (.-ok response)
                       (.json response)
                       (throw (js/Error. (str "Failed to fetch recording: " recording-file))))))
            (.then (fn [recording-json]
                     (let [steps (-> recording-json (js->clj :keywordize-keys true) :steps)]
                       (js/console.log (str "Mock replaying " (count steps) " steps from " recording-file))
                       ;; Just verify we have steps to replay
                       (is (> (count steps) 0) "Recording should have steps to replay")
                       (js/console.log "Mock replay completed successfully")
                       (done))))
            (.catch (fn [err]
                      (js/console.error (str "Failed to process recording " recording-file ":") err)
                      (is false (str "Failed to process recording " recording-file ": " (.-message err)))
                      (done))))))))
