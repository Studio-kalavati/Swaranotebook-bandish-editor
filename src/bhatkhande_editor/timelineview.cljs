(ns bhatkhande-editor.timelineview
  (:require
   [re-frame.core :as re-frame :refer [subscribe dispatch dispatch-sync]]
   [re-com.core :as re-com :refer [v-box h-box
                                   single-dropdown
                                   md-icon-button
                                   ]]
   [reagent.core :as reagent]
     [bhatkhande-editor.events :as events]
     [bhatkhande-editor.db :as db]
     [bhatkhande-editor.subs :as subs]
     ))

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
                                  (dispatch [::events/drag-segment @dragging-handle orig-delta-percent])
                                  (reset! drag-start-x orig-clamped-client-x)))))

        handle-mouse-up (fn []
                          (when @dragging-handle
                            (dispatch [::events/end-drag-segment])
                            (reset! dragging-handle nil)
                            (reset! drag-start-x nil)))

        format-time (fn [seconds]
                      (let [mins (int (/ seconds 60))
                            secs (mod (int seconds) 60)]
                        (str mins ":" (when (< secs 10) "0") secs)))

        handle-document-click (fn [e]
                                (when-let [container @container-ref]
                                  (when-not (.contains container (.-target e))
                                    (dispatch [::events/hide-timeline-dropdown]))))]

    (fn []
      (let [segments @(subscribe [::subs/timeline-segments])
            youtube-sync @(subscribe [::subs/youtube-sync?])
            part-titles @(subscribe [::subs/part-titles])
            segment-parts @(subscribe [::subs/timeline-segment-parts])
            selected-segment @(subscribe [::subs/selected-timeline-segment])]
        (when youtube-sync
          (let [total-duration (or @(subscribe [::subs/youtube-video-duration]) 0)
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
            (dispatch [::events/set-time-ranges time-ranges])
            [:div
             {:class "timeline-container"
              :ref #(when (identity %)
                      (reset! container-width (.-offsetWidth %))
                      (reset! container-ref %)
                      (.addEventListener js/document "click" handle-document-click))
              :on-mouse-up handle-mouse-up
              :on-mouse-leave handle-mouse-up
              :on-mouse-move handle-mouse-move}

             (doall
              (map-indexed
               (fn [segment-index segment-percent]
                 (let [color (if (even? segment-index) db/timeline-blue db/timeline-green)
                       is-last? (= segment-index (dec (count segments)))
                       [start-time end-time] (nth time-ranges segment-index)
                       is-dragging? (= @dragging-handle segment-index)
                       is-selected? (= segment-index selected-segment)
                       on-click (fn [_] (dispatch [::events/select-timeline-segment segment-index]))]
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
                                       (mapv (fn [pt] {:id pt :label pt}) part-titles))]
                [h-box
                 :align :center
                 :justify :center
                 :gap "5px"
                 :children [[md-icon-button
                             :md-icon-name "zmdi zmdi-play zmdi-hc-lg"
                             :on-click #(dispatch [::events/start-youtube-video-from selected-start-time selected-end-time])]
                            [md-icon-button
                             :md-icon-name "zmdi zmdi-chevron-down zmdi-hc-lg"
                             :on-click #(dispatch [::events/toggle-timeline-dropdown selected-segment])]
                            [md-icon-button
                             :md-icon-name "zmdi zmdi-plus zmdi-hc-lg"
                             :on-click #(dispatch [::events/split-timeline-segment selected-segment])]
                            [single-dropdown
                             :choices part-choices
                             :model selected-part
                             :width "100px"
                             :on-change
                             #(dispatch [::events/set-timeline-segment-part selected-segment %])]]])]]))))))
