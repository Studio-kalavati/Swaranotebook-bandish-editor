(ns bhatkhande-svg-viewer.views
  (:require
   [re-frame.core :as re-frame :refer [subscribe dispatch dispatch-sync]]
   [re-com.core :as re-com :refer [at]]
   [breaking-point.core :as bp]
   [reagent.core :as reagent]
   [bhatkhande-svg-viewer.events :as events]
   [bhatkhande-svg-viewer.routes :as routes]
   [bhatkhande-svg-viewer.subs :as subs]))

(def img-path "images/swaras/")
(defn get-image-map
  [langpath sa-pitch]
  (if (= "cdef" langpath)
    (zipmap (subvec note-seq 24 (+ 24 48))
            (mapv
             #(str img-path langpath "/png/" % ".png" )
             (range 1 49)))
    (let [indx (.indexOf note-seq sa-pitch)
          start-indx (- indx 12)]
      ;;(println " image-map " indx " start-indx " start-indx " take 20 " (take 30 note-seq))
      (zipmap (subvec note-seq start-indx (+ start-indx 37))
              (mapv
               #(str img-path langpath "/png/" % ".png" )
               (range 1 38))))))

(defn svara-wav-display-panel
  []
  (let [loaded? (reagent/atom false)
        dim (reagent/atom nil)
        font-size (reagent/atom 20)]
    (fn []
      (let [{:keys [length ] :as midi-data} @(subscribe [::subs/midi-data])
            comp-data @(subscribe [::subs/compdata])
            mid-seq (-> comp-data :midi-data :midi-seq de-serialize-midi-seq)
            bandish-mode true 
            taal-def (when bandish-mode
                       (-> comp-data :viz-metadata :taal keyword taal-map))
            lang (or @(subscribe [::subs/lang]) :cdef)
            langpath (name lang)
            sa-pitch @(subscribe [::subs/sa-pitch])
            img-map (get-image-map langpath sa-pitch)
            path-fn (path-xform img-map)
            svara-lines @(subscribe [::subs/svara-disp-seq])
            newline-on-bhaag? @(subscribe [::subs/newline-on-bhaag?])
            viz-props {:taal-def taal-def
                       :newline-on-bhaag? newline-on-bhaag?}
            svara-xy (->> svara-lines
                          path-fn
                          add-index-by-group
                          (remove #(#{"[" "]"} (:t %)))
                          add-group-keys-display
                          (add-xy-to-vizseq viz-props))
            xy-pos (->> svara-xy (mapv :xy))
            start-offset 0
            sv-time (mapv second mid-seq)
            note-dur (mapv last mid-seq)
            m-space @(subscribe [::subs/min-space])
            hw {:height m-space :width m-space}
            last-y (-> svara-xy last :xy last)
            max-x (->> svara-xy (map (comp first :xy)) (apply max))
            imgs (add-svg-images hw svara-xy)
            textseq (if bandish-mode
                      (bandish-svg-decorators taal-def svara-xy)
                      (add-concatenated-text svara-xy))
            svg-width (+ (* 2 m-space) max-x)]
        [:div {:class "choose-song-area inner"
               :ref
               #(if (identity %)
                 (let [dim (client-dim %)]
                   (when-not @loaded?
                     (dispatch [::events/set-dim dim])
                     (reset! loaded? true))))}
         [:div {:class "music-notes"
                :ref #(when (and (not (vector? @dim)) (identity %))
                          (dispatch [::events/set-music-notes-element %])
                          (let [cdim (client-dim %)]
                            (reset! dim cdim)))}
          (let [[w h] @(subscribe [::subs/dim])
                svg-height (+ (* 10 m-space) last-y)]
              (-> [:svg {:height svg-height
                         :width svg-width
                         ;;:viewBox (str "0 200 " svg-width " " svg-height)
                         :xmlns "http://www.w3.org/2000/svg"}]
                  (into imgs)
                  (into textseq)))]
         [gap :size "1vh"]
         [h-box
          :gap "2vw"
          :justify :center
          :children [[button :label "Change Language"
                      :style {:width "10vw"}
                            :on-click
                      #(dispatch [::events/toggle-lang])]
                     [button :label "Font ++"
                      :style {:width "10vw"}
                      :on-click
                      #(do (dispatch [::events/set-min-space  (fn[i] (+ i 5))]))]
                     [button :label "Font --"
                      :style {:width "10vw"}
                      :on-click
                      #(do (dispatch [::events/set-min-space  (fn[i] (- i 5))]))]
                     [button :label "Download"
                      :style {:width "10vw"}
                      :class    "btn btn-primary"
                      :on-click (fn[_] (download-link comp-data))]]]]))))

(defn alternating-color-background
  "returns svg data with alternating colours for the background"
  [svg-width hw svara-xy]
  (->>
   svara-xy
   (map (comp second :xy ))
   set
   sort
   (map vector (range))
   (mapv (fn[[a y]]
           (vector :rect {:width svg-width :height (:height hw)
                          :fill (if (even? a) "white" "#f2f2f2")
                          :y y
                          :rx 1
                          :x 0})))))

(defn download-link
  [compdata]
  (let [w (t/writer :json)
        json (t/write w compdata)
        blob (js/Blob. [json] #js {"type" "octet/stream"})
        url (.createObjectURL js/URL blob)]
    (.assign (.-location js/window) url)))

(defn svara-wav-display-panel
  []
  (let [loaded? (reagent/atom false)
        dim (reagent/atom nil)
        audio-element-set? (reagent/atom false)
        font-size (reagent/atom 20)]
    (fn []
      (let [{:keys [length ] :as midi-data} @(subscribe [::subs/midi-data])
            comp-data @(subscribe [::subs/compdata])
            mid-seq (-> comp-data :midi-data :midi-seq de-serialize-midi-seq)
            bandish-mode (when-let [vz (-> comp-data :viz-metadata)]
                           (= "bandish" (name (get vz :mode))))
            taal-def (when bandish-mode
                       (-> comp-data :viz-metadata :taal keyword taal-map))
            lang (or @(subscribe [::subs/lang]) :cdef)
            langpath (name lang)
            sa-pitch @(subscribe [::subs/sa-pitch])
            img-map (get-image-map langpath sa-pitch)
            path-fn (path-xform img-map)
            svara-lines @(subscribe [::subs/svara-disp-seq])
            newline-on-bhaag? @(subscribe [::subs/newline-on-bhaag?])
            viz-props {:taal-def taal-def
                       :newline-on-bhaag? newline-on-bhaag?}
            _ (println " wav-props viz-props  " viz-props " mode " bandish-mode
                       " viz-meta "(-> comp-data :viz-metadata))
            svara-xy (if bandish-mode
                       (->> svara-lines
                            path-fn
                            add-index-by-group
                            (remove #(#{"[" "]"} (:t %)))
                            add-group-keys-display
                            (add-xy-to-vizseq viz-props))
                       (->> svara-lines path-fn
                            add-xy))
            xy-pos (->> svara-xy (mapv :xy))
            start-offset 0
            sv-time (mapv second mid-seq)
            note-dur (mapv last mid-seq)
            m-space @(subscribe [::subs/min-space])
            hw {:height m-space :width m-space}
            last-y (-> svara-xy last :xy last)
            max-x (->> svara-xy (map (comp first :xy)) (apply max))
            imgs (add-svg-images hw svara-xy)
            textseq (if bandish-mode
                      (bandish-svg-decorators taal-def svara-xy)
                      (add-concatenated-text svara-xy))
            svg-width (+ (* 2 m-space) max-x)
            color-lines (alternating-color-background svg-width hw svara-xy)]
        [:div {:class "choose-song-area inner"
               :ref
               #(if (identity %)
                 (let [dim (client-dim %)]
                   (when-not @loaded?
                     (dispatch [::events/set-dim dim])
                     (reset! loaded? true))))}
         [:div {:class "music-notes"
                :ref #(when (and (not (vector? @dim)) (identity %))
                          (dispatch [::events/set-music-notes-element %])
                          (let [cdim (client-dim %)]
                            (reset! dim cdim)))
                  :style {:max-height "75vh"
                          :scroll-behavior "smooth"
                          :scrollbar-width "none"
                          ;:background-image "url('images/backgrounds/mountain.jpg')"
                          ;:background-size "cover"
                          :overflow-y "auto"}}
          (let [[w h] @(subscribe [::subs/dim])
                svg-height (+ (* 10 m-space) last-y)]
              (dispatch [::events/save-svara-timeseq (vec sv-time) xy-pos note-dur])
              (dispatch [::events/reset-sv-pos-map])
              (-> [:svg {:height svg-height
                         :width svg-width
                         ;;:viewBox (str "0 200 " svg-width " " svg-height)
                         :xmlns "http://www.w3.org/2000/svg"}]
                  (into color-lines)
                  (into imgs)
                  (into textseq)))]
         [gap :size "1vh"]
         [h-box
          :justify :center
          :children
          [[:div [:audio
                  {:ref #(when (and (identity %) (not @audio-element-set?))
                           (do
                             (reset! audio-element-set? true)
                             (dispatch [::events/set-audio-elem %])))
                   :controls true
                   :src @(subscribe [::subs/audio-buffer])}]]]]
         [gap :size "1vh"]
         [h-box
          :gap "2vw"
          :justify :center
          :children [[button :label "Change Language"
                      :style {:width "10vw"}
                            :on-click
                      #(dispatch [::events/toggle-lang])]
                     [button :label "Font ++"
                      :style {:width "10vw"}
                      :on-click
                      #(do (dispatch [::events/set-min-space  (fn[i] (+ i 5))]))]
                     [button :label "Font --"
                      :style {:width "10vw"}
                      :on-click
                      #(do (dispatch [::events/set-min-space  (fn[i] (- i 5))]))]
                     [button :label "Download"
                      :style {:width "10vw"}
                      :class    "btn btn-primary"
                      :on-click (fn[_] (download-link comp-data))]
                     [button :label "Submit"
                      :style {:width "10vw"}
                      :class "btn btn-primary"
                      :on-click
                      #(do
                         (dispatch [::events/mark-editing-complete])
                         (dispatch [::events/goto-page :list-comps-panel]))]]]]))))

(defmethod routes/panels :home-panel [] [svara-wav-display-panel])

(defn main-panel []
  (let [active-panel (re-frame/subscribe [::subs/active-panel])]
    [re-com/v-box
     :src      (at)
     :height   "100%"
     :children [(routes/panels @active-panel)]]))
