(ns puppeteer.test-setup)

;; This will be executed before tests run
(defn ^:export init []
  ;; Polyfill for 'global' in browser environment
  (when (and (exists? js/window) (not (exists? js/global)))
    (set! js/global js/window)
    (js/console.log "Added global polyfill for browser environment")))

;; Call init immediately
(init)
