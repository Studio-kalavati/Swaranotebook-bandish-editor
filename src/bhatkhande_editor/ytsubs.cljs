(ns bhatkhande-editor.ytsubs
  (:require
   [re-frame.core :as re-frame :refer [reg-sub]]))

(reg-sub
  ::youtube-sync?
  :<- [::props]
  (fn [props [_ _]]
    (:youtube-sync props)))

(reg-sub
   ::youtube-video-id
   :<- [::props]
   (fn [props [_ _]]
     (:youtube-video-id props)))

(reg-sub
 ::youtube-player-state
 :<- [::props]
 (fn [props [_ _]]
   (:youtube-player-state props)))

(reg-sub
   ::youtube-player
   :<- [::props]
   (fn [props [_ _]]
     (:youtube-player props)))

(reg-sub
   ::youtube-video-duration
   :<- [::props]
   (fn [props [_ _]]
     (:youtube-video-duration props)))

(reg-sub
   ::timeline-segments
   :<- [::props]
   (fn [props [_ _]]
     (:timeline-segments props)))

(reg-sub
  ::timeline-segment-count
  :<- [::props]
  (fn [props _]
    (:timeline-segment-count props)))

(reg-sub
  ::dragging-timeline-segment
  :<- [::props]
  (fn [props _]
    (:dragging-timeline-segment props)))

(reg-sub
   ::timeline-colors
   :<- [::props]
   (fn [props _]
     (:timeline-colors props)))

(reg-sub
   ::timeline-segment-parts
   :<- [::props]
   (fn [props _]
     (:timeline-segment-parts props)))

(reg-sub
   ::selected-timeline-segment
   :<- [::props]
   (fn [props _]
     (:selected-timeline-segment props)))

(reg-sub
 ::show-video-change-modal
 :<- [::props]
 (fn [props _]
   (:show-video-change-modal props)))
