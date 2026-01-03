(ns bhatkhande-editor.youtube
  (:require [bhatkhande-editor.utils :refer [extract-youtube-video-id]]
            [bhatkhande-editor.events :as events]
            ))

(def ^:private player-instance (atom nil))
(def ^:private api-ready? (atom false))
(def ^:private pending-create-requests (atom []))
(declare create-player!)
(defn get-player []
  @player-instance)

(defn set-player! [player]
  (reset! player-instance player))

(defn destroy-player! []
  (when-let [p @player-instance]
    (.destroy ^js/YT.player p))
  (reset! player-instance nil))

(defn check-api-ready []
  (when (and js/YT (.-Player js/YT))
    (reset! api-ready? true)
    (doseq [request @pending-create-requests]
      (apply create-player! request))
    (reset! pending-create-requests [])))

(defn create-player! [dom-id video-id dispatch-fn]
  (when @player-instance
    (destroy-player!))
  (if @api-ready?
    (let [player (js/YT.Player. dom-id
                                #js {:height "50%"
                                     :width "100%"
                                     :videoId video-id
                                     :playerVars #js {:playsinline 1 :rel 0}
                                     :events #js {:onReady
                                                  (fn [event]
                                                    (let [player (.-target event)]
                                                      (set-player! player)
                                                      (dispatch-fn [::events/set-youtube-player player])
                                                      (let [duration (.getDuration ^js/YT.player player)]
                                                        (when (and duration (> duration 0))
                                                          (dispatch-fn [::events/set-youtube-video-duration duration])))))
                                                   :onError (fn [event]
                                                              (println " received on error"))
                                                   :onStateChange (fn [e]
                                                                    (let [state (.-data e)]
                                                                      (dispatch-fn [::events/youtube-state-change state])))
                                                  ;; -1 (unstarted) 0 (ended) 1 (playing) 2 (paused) 3 (buffering) 5 (video cued)
                                                  }})]
      player)
    (do
      (swap! pending-create-requests conj [dom-id video-id dispatch-fn])
      (set! (.-onYouTubeIframeAPIReady js/window)
            (fn []
              (check-api-ready)))
      nil)))

(defn load-video! [player video-id]
  (when player
    (.loadVideoById ^js/YT.player player video-id)))
