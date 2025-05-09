(ns puppeteer.replay-test
  (:require [cljs.test :refer-macros [deftest is testing async]]
            [cljs.core.async :refer [go]]))

;; Simple test that loads a recording JSON file
(deftest load-recording-test
  (testing "Loading a recording from puppeteer_recordings"
    (async done
      (let [recording-file "add_svara.json"
            recording-url (str "/base/puppeteer_recordings/" recording-file)]
        (-> (js/fetch recording-url)
            (.then (fn [response]
                     (if (.-ok response)
                       (.json response)
                       (throw (js/Error. (str "Failed to fetch recording: " recording-file))))))
            (.then (fn [recording-json]
                     (js/console.log (str "Successfully loaded recording: " recording-file))
                     (is (map? (js->clj recording-json)) "Recording should be a valid JSON object")
                     ;;AI! test the assertions in the replay and fail if an assertion fails
                     (is (contains? (js->clj recording-json) "steps") "Recording should contain steps")
                     (done)))
            (.catch (fn [err]
                      (js/console.error (str "Failed to fetch recording " recording-file ":") err)
                      (is false (str "Failed to fetch recording " recording-file ": " (.-message err)))
                      (done))))))))
