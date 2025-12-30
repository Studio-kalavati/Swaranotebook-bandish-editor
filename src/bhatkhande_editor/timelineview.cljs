(ns bhatkhande-editor.timelineview
  (:require
   [re-frame.core :as re-frame :refer [subscribe dispatch dispatch-sync]]
   [re-com.core :as re-com :refer [title
                                   hyperlink v-box h-box
                                   hyperlink-href
                                   single-dropdown
                                   checkbox
                                   line
                                   label
                                   radio-button
                                   slider
                                   box
                                   input-text
                                   md-icon-button
                                   ;;simple-v-table
                                   at
                                   button
                                   gap
                                   throbber
                                   modal-panel]]
   [re-com.util     :refer [item-for-id]]
   [breaking-point.core :as bp]
   [re-pressed.core :as rp]
   [sargam.ragas :refer [varjit-svaras]]
   [sargam.talas :refer [taal-def]]
   [sargam.languages :refer [lang-labels]]
   [sargam.spec :as us]
   [reagent.core :as reagent]
   [cognitect.transit :as t]
   [clojure.string :as cstring]
     [bhatkhande-editor.events :as events]
     [bhatkhande-editor.routes :as routes]
     [bhatkhande-editor.db :as db :refer [mswaras pitch-options-list get-sahitya]]
     [bhatkhande-editor.subs :as subs]
     [bhatkhande-editor.utils :as utils]
     [bhatkhande-editor.youtube :as youtube]))

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
                                    delta-x (- clamped-client-x @drag-start-x)
                                    delta-percent (* 100.0 (/ delta-x @container-width))
                                    orig-delta-percent (* 100.0 (/ (- orig-clamped-client-x @drag-start-x) @container-width)) ]
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
            youtube-sync @(subscribe [::subs/youtube-sync])
            part-titles @(subscribe [::subs/part-titles])
            segment-parts @(subscribe [::subs/timeline-segment-parts])
            visible-dropdown @(subscribe [::subs/visible-timeline-dropdown])]
        (when youtube-sync
          (let [total-duration (or @(subscribe [::subs/youtube-video-duration]) 0)
                cumulative-percentages (reductions + segments)
                time-ranges (map-indexed
                            (fn [idx cumulative-percent]
                              (let [start-time (if (= idx 0)
                                                 0
                                                 (/ (* (nth cumulative-percentages (dec idx)) total-duration) 100))
                                    end-time (/ (* cumulative-percent total-duration) 100)]
                                [start-time end-time]))
                            cumulative-percentages)]
            [:div
             {:style {:width "100%"
                      :max-width "100%"
                      :padding "40px 0 15px 0"
                      :margin-top "10px"
                      :position "relative"
                      :border-radius "8px"
                      :background-color "#f5f5f5"}
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
                       onclick (fn[_] (dispatch [::events/start-youtube-video-from start-time end-time])) 
                       selected-part (get segment-parts segment-index)
                       part-choices (into [{:id "" :label "None"}] 
                                         (mapv (fn[pt] {:id pt :label pt}) part-titles))]
                    [:div
                     {:key (str "segment-" segment-index)
                      :style {:display "inline-block"
                              :height "10px"
                              :width (str segment-percent "%")
                              :background-color color
                              :opacity (if (or is-dragging? 
                                           (= @dragging-handle (dec segment-index)))
                                       0.7 1)
                              :border-radius (if (= segment-index 0) "8px 0 0 8px"
                                             (if is-last? "0 8px 8px 0" "0"))
                              :position "relative"
                              :transition "opacity 0.15s ease"
                              :vertical-align "top"}
                      ;;:on-click onclick
                      }
                    
                     [:div
                      {:style {:position "absolute"
                               :bottom "100%"
                               :left "0"
                               :width "1px"
                               :height "25px"
                               :background-color "black"
                               :pointer-events "none"}}
                     [:div
                      {:style {:position "absolute"
                               :bottom "0"
                               :left "5px"
                               :color "black"
                               :font-size "11px"
                               :font-weight "bold"
                               :white-space "nowrap"
                               :pointer-events "none"}}
                      (format-time start-time)]
                    
                     [:div
                       {:style {:position "absolute"
                                :bottom "15px"
                                :left "0"
                                :z-index 1000
                                :pointer-events "auto"}}
                       [h-box
                        :gap "5px"
                        :children [
                          [md-icon-button
                           :md-icon-name "zmdi zmdi-play zmdi-hc-lg"
                           :on-click #(do
                                       (.stopPropagation %)
                                       (dispatch [::events/start-youtube-video-from start-time end-time]))]
                          [md-icon-button
                           :md-icon-name "zmdi zmdi-chevron-down zmdi-hc-lg"
                           :on-click #(do
                                       (.stopPropagation %)
                                       (dispatch [::events/toggle-timeline-dropdown segment-index]))]
                          [md-icon-button
                           :md-icon-name "zmdi zmdi-plus zmdi-hc-lg"
                           :on-click #(do
                                       (.stopPropagation %)
                                       (dispatch [::events/split-timeline-segment segment-index]))]]]
                     (when (= visible-dropdown segment-index)
                       [:div
                         {:style {:position "absolute"
                                  :top "100%"
                                  :left "0"
                                  :z-index 2000}}
                        [single-dropdown
                         :choices part-choices
                         :model selected-part
                         :width "100px"
                         :on-change #(dispatch [::events/set-timeline-segment-part segment-index %])]])]
                     ]
                    
                    (when-not is-last?
                      [:div
                       {:style {:position "absolute"
                                :right "-8px"
                                :top "50%"
                                :transform "translateY(-50%)"
                                :width "16px"
                                :height "16px"
                                :background-color "white"
                                :border (str "3px solid " color)
                                :border-radius "50%"
                                :cursor "ew-resize"
                                :z-index 10
                                :box-shadow (if is-dragging?
                                             "0 0 10px rgba(0,0,0,0.5)"
                                             "0 2px 5px rgba(0,0,0,0.3)")
                                :transition "box-shadow 0.15s ease"}
                        :on-mouse-down #(handle-mouse-down segment-index %)}])]))
               segments))]))))))
