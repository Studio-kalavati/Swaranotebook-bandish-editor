(ns puppeteer.test-setup)

;; Define global before any code runs
(js* "if (typeof window !== 'undefined' && typeof global === 'undefined') { window.global = window; console.log('Added global polyfill for browser environment'); }")

;; This will be executed before tests run
(defn ^:export init []
  ;; Additional setup can go here
  (js/console.log "Test setup initialized"))

;; Call init immediately
(init)
