(ns bhatkhande-editor.ytevents
  (:require
   [re-frame.core :as re-frame
    :refer [reg-event-db reg-event-fx
            dispatch]]
   [chronoid.core :as c]
   [bhatkhande-editor.db :as db ]
   [bhatkhande-editor.utils :as utils :refer [get-clock]]
   ))

(reg-event-db
   ::set-timeline-segment-part
   (fn [db [_ segment-index part-title]]
    (update-in db [:props :timeline-segment-parts segment-index] (constantly part-title))))

(reg-event-db
   ::select-timeline-segment
   (fn [db [_ segment-index]]
     (-> db
         (assoc-in [:props :selected-timeline-segment] segment-index))))

(reg-event-db
  ::delete-timeline-segment
  (fn [db [_ segment-index]]
    (let [segments (get-in db [:props :timeline-segments])]
      (if (or (<= (count segments) 1) (= segment-index 0))
        db
        (let [segment-percent (nth segments segment-index)
              left-idx (dec segment-index)
              left-percent (nth segments left-idx)
              new-left-percent (+ left-percent segment-percent)
              before (subvec segments 0 left-idx)
              middle [new-left-percent]
              after (subvec segments (inc segment-index))
              new-segments (vec (concat before middle after))
              segment-parts (get-in db [:props :timeline-segment-parts])
              before-parts (subvec segment-parts 0 left-idx)
              after-parts (subvec segment-parts (inc segment-index))
              new-parts (vec (concat before-parts after-parts))]
          (-> db
              (assoc-in [:props :timeline-segments] new-segments)
              (assoc-in [:props :timeline-segment-parts] new-parts)
              (assoc-in [:props :selected-timeline-segment] left-idx)))))))

(reg-event-db
 ::split-timeline-segment
 (fn [db [_ segment-index]]
   (let [segments (get-in db [:props :timeline-segments])
         segment-percent (nth segments segment-index)
         half-percent (/ segment-percent 2)
         before (subvec segments 0 segment-index)
         after (subvec segments (inc segment-index))
         new-segments (vec (concat before [half-percent half-percent] after))
         segment-parts (get-in db [:props :timeline-segment-parts])
         before-parts (if (= segment-index 0) segment-parts (subvec segment-parts 0 segment-index))
         after-parts (if (= 0 (count segment-parts)) [] (subvec segment-parts (inc segment-index)))
         new-parts (vec (concat before-parts [nil nil] after-parts))]
     (-> db
         (assoc-in [:props :timeline-segments] new-segments)
         (assoc-in [:props :timeline-segment-parts] new-parts)))))

(reg-event-db
  ::set-time-ranges
  (fn [db [_ time-ranges]]
    ;;a vector of length equal to the number of segments
    ;;each one is a 2-tuple of [start-time end-time]
    (assoc-in db [:props :time-ranges] time-ranges)))

(reg-event-db
 ::drag-segment
 (fn [db [_ handle-index delta-percent]]
   (let [segments (get-in db [:props :timeline-segments])
         left-idx handle-index
         right-idx (inc handle-index)
         min-percent db/min-segment-percent
         max-percent db/max-segment-percent
         current-left (nth segments left-idx)
         current-right (nth segments right-idx)
         new-left-percent (max min-percent
                               (min max-percent
                                    (+ current-left delta-percent)))
         new-right-percent (max min-percent
                                (min max-percent
                                     (- current-right delta-percent)))]
     (-> db
         (update-in [:props :timeline-segments]
                    assoc left-idx new-left-percent)
         (update-in [:props :timeline-segments]
                    assoc right-idx new-right-percent)))))

(reg-event-db
 ::start-drag-segment
 (fn [db [_ handle-index]]
   (assoc-in db [:props :dragging-timeline-segment] handle-index)))

(reg-event-db
 ::end-drag-segment
 (fn [db [_ _]]
   (assoc-in db [:props :dragging-timeline-segment] nil)))

(reg-event-fx
 ::pause-youtube-video
 (fn [{:keys [db]} _]
   (let [player (get-in db [:props :youtube-player])]
     (when player
       (.pauseVideo ^js/YT.Player player)))
   {}))

(defn play-yt-video
  [player params]
  (when player
    (.loadVideoById ^js/YT.player player params)
    (.playVideo  ^js/YT.Player player)))

(reg-event-fx
 ::start-youtube-video-from
 (fn [{:keys [db]} [_ from to]]
   (let [player (get-in db [:props :youtube-player])
         youtube-video-id (get-in db [:props :youtube-video-id])
         params #js {:videoId youtube-video-id
                     :startSeconds from :endSeconds to}]
     (play-yt-video player params))
   {}))

(reg-event-fx
 ::play-youtube-video-from-start
 (fn [{:keys [db]} _]
   (let [player (get-in db [:props :youtube-player])
         youtube-video-id (get-in db [:props :youtube-video-id])
         params #js {:videoId youtube-video-id :startSeconds 0}]
     (play-yt-video player params))
   {}))

(reg-event-fx
 ::youtube-sync-play
 (fn [{:keys [db]} [_ [start-time end-time]]]
   (let [ndb (if-not (:audio-context db) (merge db (get-clock)) db)
         {:keys [clock]} ndb]
     {:db (-> ndb
              (update-in
               [:timer]
               (constantly
                (-> (c/set-timeout!
                     clock
                     #(dispatch
                       [::youtube-clock-tick-event]) 0)
                    (c/repeat! 400)))))
      :dispatch
      (if (and start-time end-time)
        [::start-youtube-video-from start-time end-time]
        [::start-youtube-video-from start-time (-> db :props :youtube-video-duration)])})))

(reg-event-fx
 ::show-video-change-modal
 (fn [{:keys [db]} [_ show?]]
   {:db (update-in db [:props :show-video-change-modal] (constantly show?))}))
