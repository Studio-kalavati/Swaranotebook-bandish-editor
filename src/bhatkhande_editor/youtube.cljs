(ns bhatkhande-editor.youtube
  (:require [bhatkhande-editor.events :as events]
            [re-frame.core :as re-frame :refer [subscribe dispatch]]
            [bhatkhande-editor.subs :as subs]
            [reagent.core :as reagent]
            [bhatkhande-editor.utils :as utils]
            [re-com.core :as re-com :refer [title
                                            v-box h-box
                                            input-text
                                            button
                                            gap
                                            modal-panel]]))

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
  (if (or @api-ready? (and js/YT (.-Player js/YT)))
    (do
      (when (and js/YT (.-Player js/YT))
        (reset! api-ready? true))
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
        player))
    (do
      (swap! pending-create-requests conj [dom-id video-id dispatch-fn])
      (set! (.-onYouTubeIframeAPIReady js/window)
            (fn []
              (check-api-ready)))
      nil)))

(defn load-video! [player video-id]
  (when player
    (.loadVideoById ^js/YT.player player video-id)))

(defn youtube-box
  [youtube-video-id]
  (let [player-id "youtube-player"]
    (reagent/create-class
     {:display-name "YouTubeBox"

      :component-did-mount
      (fn []
        (when youtube-video-id
          (let [existing-player (get-player)
                player-element (.getElementById js/document player-id)
                is-iframe? (and player-element
                                (= "IFRAME" (.-tagName player-element)))]

            (if (and existing-player
                     is-iframe?
                     (.getIframe ^js/YT.player existing-player))
              (load-video! existing-player youtube-video-id)
              (create-player! player-id youtube-video-id dispatch)))))

      :component-did-update
      (fn [this old-argv]
        (when youtube-video-id
          (let [new-argv (reagent/argv this)
                old-video-id (when old-argv (second old-argv))
                new-video-id (second new-argv)
                player (get-player)
                player-element (.getElementById js/document player-id)
                is-iframe? (and player-element
                                (= "IFRAME" (.-tagName player-element)))]
            (when (and (not= new-video-id old-video-id)
                       player
                       is-iframe?)
              (load-video! player new-video-id)))))

      :reagent-render
      (fn [_]
        [:div
         {:id player-id
          :style {:width "100%"
                  :position "relative"}}])})))

(defn youtube-iframe-box
  []
  (let [new-video-url (reagent/atom "")]
    (fn []
      (let [video-id @(subscribe [::subs/youtube-video-id])]
        [:div
         [h-box
          :justify :center
          :align :center
          :children
          [[youtube-box video-id]]]
         (when (and @(subscribe [::subs/show-video-change-modal])
                    (not= :play @(subscribe [::subs/mode])))
           [modal-panel
            :backdrop-on-click #(dispatch [::events/show-video-change-modal false])
            :child [:div {:style {:min-width "min(80vw,400px)"}}
                    [v-box
                     :gap "2vh"
                     :class "body"
                     :align :center
                     :children
                     [[title :level :level3 :label "Change YouTube Video"]
                      [gap :size "2vh"]
                      [input-text
                       :model new-video-url
                       :on-change #(reset! new-video-url %)
                       :style {:width "100%"
                               :justify-content "center"
                               :text-align "center"}]
                      [gap :size "2vh"]
                      (when (and (not (empty? @new-video-url))
                                 (not (utils/extract-youtube-video-id @new-video-url)))
                        [title :level :level4 :label "Invalid YouTube URL"
                         :style {:color "red"}])
                      [gap :size "2vh"]
                      [h-box
                       :gap "2vw"
                       :children
                       [[button
                         :label "OK"
                         :class "btn-hc-lg btn-primary"
                         :on-click #(let [extracted-id (utils/extract-youtube-video-id @new-video-url)]
                                      (if extracted-id
                                        (do
                                          (println " extracted ytid " extracted-id)
                                          (dispatch [::events/set-youtube-video-id extracted-id])
                                          (dispatch [::events/show-video-change-modal false])
                                          (reset! new-video-url ""))
                                        (println "Invalid YouTube URL")))]
                        [button
                         :label "Cancel"
                         :class "btn-default"
                         :on-click #(do
                                      (dispatch [::events/show-video-change-modal false])
                                      (reset! new-video-url ""))]]]]]]])]))))
