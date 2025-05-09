(ns bhatkhande-editor.puppeteer.test-setup
  (:require [cljs.core.async :refer [go timeout <!]]
            [cljs.test :refer-macros [async]]
            ["http" :as http]
            ["serve-static" :as serve-static]
            ["finalhandler" :as finalhandler]))

(goog-define SERVER-PORT 8280)

(def server-instance (atom nil))

(defn start-server [done]
  (let [serve (serve-static "resources/public")
        server (http/createServer 
                (fn [req res]
                  (let [done (finalhandler req res)]
                    (serve req res done))))]
    
    (.listen server SERVER-PORT
             (fn []
               (js/console.log (str "Test server started on port " SERVER-PORT))
               (reset! server-instance server)
               (done)))
    
    ;; Return a promise that resolves when the server is started
    (js/Promise. (fn [resolve _]
                   (go
                     (<! (timeout 100)) ;; Give the server a moment to start
                     (resolve))))))

(defn stop-server [done]
  (when-let [server @server-instance]
    (.close server
            (fn []
              (js/console.log "Test server stopped")
              (reset! server-instance nil)
              (done)))
    
    ;; Return a promise that resolves when the server is stopped
    (js/Promise. (fn [resolve _]
                   (go
                     (<! (timeout 100)) ;; Give the server a moment to stop
                     (resolve)))))
  
  ;; If no server is running, just resolve immediately
  (when (nil? @server-instance)
    (done)
    (js/Promise.resolve)))
