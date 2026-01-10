(ns bhatkhande-editor.timelineview
  (:require
   [re-frame.core :as re-frame :refer [subscribe dispatch]]
   [re-com.core :as re-com :refer [h-box
                                   title
                                   box
                                   single-dropdown
                                   md-icon-button]]
   [reagent.core :as reagent]
   [bhatkhande-editor.ytevents :as ytevents]
   [bhatkhande-editor.events :as events]
   [bhatkhande-editor.db :as db]
   [bhatkhande-editor.ytsubs :as ytsubs]
   [bhatkhande-editor.subs :as subs]))

(defn timeline-view
  []
  (let [dragging-handle (reagent/atom nil)
        drag-start-x (reagent/atom nil)
        container-width (reagent/atom 0)
        container-ref (reagent/atom nil)

        handle-mouse-down (fn [handle-index e]
                            (reset! dragging-handle handle-index)
                            (reset! drag-start-x (.-clientX e))
                            (.preventDefault e)
                            (.stopPropagation e))

        handle-mouse-move (fn [e]
                            (when @dragging-handle
                              (let [container-rect (.getBoundingClientRect @container-ref)
                                    max-client-x (+ (.-left container-rect) (.-width container-rect))
                                    orig-clamped-client-x (min (.-clientX e) max-client-x)
                                    clamped-client-x (- (.-clientX e) (.-left container-rect))
                                    orig-delta-percent (* 100.0 (/ (- orig-clamped-client-x @drag-start-x) @container-width))]
                                (when (> @container-width (+ 10 clamped-client-x))
                                  ;;the segments overrun the width  - needs to be fixed
                                ;;(println " --  "[  orig-clamped-client-x clamped-client-x ])
                                ;;(println " 22  "[  delta-percent (* 100.0 (/ (- orig-clamped-client-x @drag-start-x) @container-width))])
                                ;;(println " 12 "[ max-client-x  delta-x delta-percent  (.-left container-rect) (.-width container-rect) (.-clientX e) @container-width])
                                  (dispatch [::ytevents/drag-segment @dragging-handle orig-delta-percent])
                                  (reset! drag-start-x orig-clamped-client-x)))))

        handle-mouse-up (fn []
                          (when @dragging-handle
                            (dispatch [::ytevents/end-drag-segment])
                            (reset! dragging-handle nil)
                            (reset! drag-start-x nil)))

        format-time (fn [seconds]
                      (let [mins (int (/ seconds 60))
                            secs (mod (int seconds) 60)
                            ms (int (* (mod seconds 1) 100))]
                        (str mins ":" (when (< secs 10) "0") secs ":" (when (< ms 10) "0") ms)))]

    (fn []
      (let [segments @(subscribe [::ytsubs/timeline-segments])
            youtube-sync @(subscribe [::ytsubs/youtube-sync?])
            play-mode? (and youtube-sync (= :play @(subscribe [::subs/mode])))
            part-titles @(subscribe [::subs/part-titles])
            segment-parts @(subscribe [::ytsubs/timeline-segment-parts])
            selected-segment @(subscribe [::ytsubs/selected-timeline-segment])]
        (when youtube-sync
          (let [total-duration (or @(subscribe [::ytsubs/youtube-video-duration]) 0)
                cumulative-percentages (reductions + segments)
                time-ranges
                (->> (map-indexed
                      (fn [idx cumulative-percent]
                        (let [start-time (if (= idx 0)
                                           0
                                           (/ (* (nth cumulative-percentages (dec idx)) total-duration) 100))
                              end-time (/ (* cumulative-percent total-duration) 100)]
                          [start-time end-time]))
                      cumulative-percentages)
                     vec)]
            (dispatch [::ytevents/set-time-ranges time-ranges])
            [:div
             {:class "timeline-container"
              :ref #(when (identity %)
                      (reset! container-width (.-offsetWidth %))
                      (reset! container-ref %))
              :on-mouse-up handle-mouse-up
              :on-mouse-leave handle-mouse-up
              :on-mouse-move (if (= :play @(subscribe [::subs/mode])) (fn [])
                                 handle-mouse-move)}

             (doall
              (map-indexed
               (fn [segment-index segment-percent]
                 (let [color (if (even? segment-index) db/timeline-blue db/timeline-green)
                       is-last? (= segment-index (dec (count segments)))
                       [start-time end-time] (nth time-ranges segment-index)
                       is-dragging? (= @dragging-handle segment-index)
                       is-selected? (= segment-index selected-segment)
                       on-click (fn [_] (dispatch [::ytevents/select-timeline-segment segment-index]))]
                   [:div
                    {:key (str "segment-" segment-index)
                     :class (str "timeline-segment"
                                 (when is-selected? " selected")
                                 (when (= segment-index 0) " first")
                                 (when is-last? " last")
                                 (when (or is-dragging?
                                           (= @dragging-handle (dec segment-index)))
                                   " dragging"))
                     :style {:width (str segment-percent "%")
                             :background-color color}
                     :on-click on-click}

                    [:div
                     {:class "timeline-tick"}]
                    [:div
                     {:class "time-label"}
                     (format-time start-time)]

                    (when-not is-last?
                      [:div
                       {:class (str "resize-handle" (when is-dragging? " dragging"))
                        :style {:border (str "3px solid " color)}
                        :on-mouse-down #(handle-mouse-down segment-index %)}])]))
               segments))

             [:div
              {:class "controls-container"}
              (let [selected-part (get segment-parts selected-segment)
                    [selected-start-time selected-end-time] (nth time-ranges selected-segment)
                    part-choices (into [{:id "" :label "None"}]
                                       (mapv (fn [pt] {:id pt :label pt}) part-titles))
                    can-delete? (and (> (count segments) 1) (> selected-segment 0))]
                [h-box
                 :align :center
                 :justify :center
                 :gap "5px"
                 :children (concat
                            [(if (= :playing @(subscribe [::ytsubs/youtube-player-state]))
                               [md-icon-button
                                :md-icon-name "zmdi zmdi-pause zmdi-hc-lg"
                                :on-click #(dispatch [::ytevents/pause-youtube-video])]
                               [md-icon-button
                                :md-icon-name "zmdi zmdi-play zmdi-hc-lg"
                                :on-click
                                #(dispatch
                                  [(if play-mode?
                                     ::ytevents/youtube-sync-play
                                     ::ytevents/start-youtube-video-from)
                                   selected-start-time selected-end-time])])
                             (when play-mode?
                               [md-icon-button
                                :md-icon-name "zmdi zmdi-collection-case-play zmdi-hc-lg"
                                :on-click
                                #(dispatch [::ytevents/youtube-sync-play])])
                             (when-not play-mode?
                               [md-icon-button
                                :md-icon-name "zmdi zmdi-plus zmdi-hc-lg"
                                :on-click #(dispatch [::ytevents/split-timeline-segment selected-segment])])
                             (when (and (not play-mode?) can-delete?)
                               [md-icon-button
                                :md-icon-name "zmdi zmdi-delete zmdi-hc-lg"
                                :on-click #(dispatch [::ytevents/delete-timeline-segment
                                                      selected-segment])])
                             [single-dropdown
                              :choices part-choices
                              :model selected-part
                              :disabled? play-mode?
                              :width "100px"
                              :on-change
                              #(dispatch [::ytevents/set-timeline-segment-part selected-segment %])]
                             [title :level :level3 :label (str "From: " (format-time selected-start-time))]
                             (when (and play-mode?
                                        (not= :playing @(subscribe [::ytsubs/youtube-player-state])))
                               [box :align :end
                                :child [md-icon-button
                                        :md-icon-name "zmdi zmdi-edit"
                                        :on-click
                                        #(do
                                           (dispatch [::events/reset-blink-style])
                                           (dispatch [::events/set-mode :edit]))]])
                             (when-not play-mode?
                               [box :align :end
                                :child [md-icon-button
                                        :md-icon-name "zmdi zmdi-youtube"
                                        :on-click
                                        #(dispatch [::ytevents/show-video-change-modal true])]])])])]]))))))
