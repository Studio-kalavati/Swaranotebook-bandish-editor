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
                     (is (contains? (js->clj recording-json) "steps") "Recording should contain steps")
                     
                     ;; Process the steps and check for assertions
                     (let [steps (-> recording-json (js->clj :keywordize-keys true) :steps)
                           assertion-steps (filter #(= (get % :type) "assertElement") steps)]
                       (js/console.log (str "Found " (count assertion-steps) " assertion steps"))
                       
                       ;; If there are assertion steps, we should validate them
                       (doseq [step assertion-steps]
                         (let [selector (get step :selector)
                               attributes (get step :attributes)]
                           (js/console.log (str "Checking assertion for selector: " selector))
                           (is (string? selector) (str "Assertion selector should be a string: " selector))
                           (is (map? attributes) (str "Assertion attributes should be a map for selector: " selector))
                           
                           ;; Check that attributes are properly defined
                           (doseq [[attr-name attr-value] attributes]
                             (is (not (nil? attr-value)) 
                                 (str "Attribute " attr-name " should have a value for selector: " selector))))))
                     
                     (done)))
            (.catch (fn [err]
                      (js/console.error (str "Failed to fetch recording " recording-file ":") err)
                      (is false (str "Failed to fetch recording " recording-file ": " (.-message err)))
                      (done))))))))
