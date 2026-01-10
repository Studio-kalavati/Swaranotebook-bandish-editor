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

(reg-event-db
 ::set-youtube-video-id
 (fn [db [_ video-id]]
   (assoc-in db [:props :youtube-video-id] video-id)))

(reg-event-db
 ::set-youtube-player
 (fn [db [_ player]]
   (assoc-in db [:props :youtube-player] player)))

(reg-event-db
 ::set-youtube-video-duration
 (fn [db [_ duration]]
   (assoc-in db [:props :youtube-video-duration] duration)))

(reg-event-db
 ::youtube-state-change
 (fn [db [_ state]]
   (let [state-keyword (case state
                         -1 :unstarted
                         0 :ended
                         1 :playing
                         2 :paused
                         3 :buffering
                         5 :video-cued
                         :unknown)]
     (assoc-in db [:props :youtube-player-state] state-keyword))))

(defn get-current-segment-index
  "returns the current segment index given the play-head of the youtube player"
  [cur-yt-time time-ranges]
  (->> time-ranges
       (keep-indexed (fn [ind [s e]]
                       (when (and (>= cur-yt-time s) (< cur-yt-time e)) ind)))
       first))

(reg-event-fx
 ::youtube-clock-tick-event
 (fn [{:keys [db]} [_ _]]
   (try
     (let [player (get-in db [:props :youtube-player])]
       (when (and player (= 1 (.getPlayerState ^js/YT.Player player)))
         (let [cur-yt-time (.getCurrentTime ^js/YT.Player player)
               cur-segment-index (->> db :props :time-ranges (get-current-segment-index cur-yt-time))
               cur-part-title (get-in db [:props :timeline-segment-parts cur-segment-index])]

           (println " csi " cur-segment-index " cpt " cur-part-title)
           ;;if there is a part associated with the current segment
           (when cur-part-title
             (let [cur-segment-setime (get-in db [:props :time-ranges cur-segment-index])
                   [start-time end-time] cur-segment-setime
                   cur-score-part-index
                   (->> db :composition
                        :score-parts
                        (keep-indexed
                         (fn [ind part]
                           (when (= (name cur-part-title) (name (:part-title part))) ind)))
                        first)
                   indexed-part (get-in db [:composition :indexed-noteseq cur-score-part-index])
                   num-avartans (count indexed-part)
                   avartan-playtime (/ (- end-time start-time) num-avartans)
                   avartan-index (->> (range num-avartans)
                                      (keep-indexed
                                       (fn [indx i]
                                         (let [s (+ start-time (* avartan-playtime i))
                                               e (+ s avartan-playtime)]
                                           (when (and (>= cur-yt-time s) (< cur-yt-time e))
                                             indx))))
                                      first)
                   same-blink-elem? (-> db :current-blink-cursor
                                        (= [cur-score-part-index avartan-index]))]
               (when-not same-blink-elem?
                 (let [blink-elems (get-in db
                                           [:blink-bhaag-index cur-score-part-index
                                            avartan-index])
                       show-lyrics? (get-in db [:props :show-lyrics])
                       font-size (get-in db [:dispinfo :font-size])]
                   (mapv
                    #(set! (.-style %)
                           (str "background-color: antiquewhite;" "max-height: "
                                (utils/bhaag-item-height show-lyrics? font-size) "px"))
                    (vals blink-elems))))
               {:db (if same-blink-elem? db
                        (update-in db [:current-blink-cursor]
                                   (constantly [cur-score-part-index avartan-index])))})))))
     (catch js/Error e
       (println " caught error in youtube-clock-tick-event" e)
       {}))))

(reg-event-fx
 ::set-youtube-sync
 (fn [{:keys [db]} [_ sync]]
   (let [ndb {:db (update-in db [:props :youtube-sync] (constantly sync))}]
     (if sync
       (assoc ndb :dispatch [::hide-onscreen-keyboard])
       ndb))))
