(ns bhatkhande-editor.views
  (:require
   [re-frame.core :as re-frame :refer [subscribe dispatch dispatch-sync]]
   [re-com.core :as re-com :refer [title
                                   hyperlink v-box h-box
                                   single-dropdown
                                   border
                                   input-textarea
                                   typeahead
                                   checkbox
                                   line
                                   radio-button
                                   label
                                   slider
                                   box
                                   input-text
                                   info-button
                                   md-icon-button
                                   md-circle-icon-button
                                   ;;simple-v-table
                                   at
                                   button
                                   gap
                                   throbber
                                   modal-panel]]
   [breaking-point.core :as bp]
   [sargam.ragas :refer [varjit-svaras]]
   [sargam.talas :refer [taal-def]]
   [sargam.languages :refer [lang-labels]]

   [reagent.core :as reagent]
   [cognitect.transit :as t]
   [bhatkhande-editor.events :as events]
   [bhatkhande-editor.routes :as routes]
   [bhatkhande-editor.db :as db :refer [note-seq mswaras]]
   [bhatkhande-editor.subs :as subs]))

(defn box-size-padding
  [viewwidth]
  (cond (< 800 viewwidth)
        ["40vw" "5vw"]
        (and (< 320 viewwidth)
             (> 600 viewwidth))
        ["70vw" "3vw"]
        (and (< 600 viewwidth)
             (> 800 viewwidth))
        ["50vw" "5vw"]
        :default
        ["80vw" "2vw"]))

(defn swar36
  "generate a vector of size 3, one each for base middle and top octaves.
  It generates the swaras for a given raga id"
  [raga-id]
  (let [varjit-set (varjit-svaras raga-id)
        com (remove varjit-set mswaras) 
        fnx (fn[saptak] (mapv #(assoc {} :note (vector saptak %)) com))
        res [(fnx :mandra) (fnx :madhyam) (fnx :taar)]]
    res))

(defn butn2
  ([text on-click-fn] (butn2 text on-click-fn {}))
  ([text on-click-fn {:keys [style class]
                      :or {style {} class "btn btn-lg swarabuttons btn-sm"}}]
   [box
    :size "auto"
    :align-self :stretch
    :style {:flex "1 1 0px"} 
    :child [:button {:style (merge style {:width "100%"}) :class class 
                     :on-click on-click-fn}
            [:span text]]]))

(defn mk-button
  ([beat-mode swaralem] (mk-button {} beat-mode swaralem))
  ([style notes-per-beat {:keys [note] :as swaralem}]
   (let [msmap @(subscribe [::subs/swaramap])]
     (butn2 (msmap (second note))
            #(do
               (dispatch [::events/conj-svara
                          {:svara swaralem :notes-per-beat @notes-per-beat}])
               #_(if (> @notes-per-beat 1)
                 (dispatch [::events/conj-part-swara
                            {:svara swaralem :notes-per-beat @notes-per-beat}])
                 (dispatch [::events/conj-single-swara [swaralem]])))
            {:style (merge style {:width "100%"})}))))

(defn box-button
  [label {:keys [disp-fn state]}]
  [box :size "1"
   :justify :center
   :align-self :stretch
   :child
   [button
    :label [:span label]
    :style {:padding "10px"}
    :class (if (true? (state)) "btn-lg swarabuttons btn-sm btn-primary btn-highlight" "btn-lg swarabuttons btn-sm btn-default")
    :on-click #(disp-fn)]])

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

(defn swara-buttons
  []
  (let [raga-info
        [v-box
         :children
         [[:p.info-heading "Choose your raga"]
          [:p "Only notes for the selected raga will be displayed for easy swara entry "]]]
        taal-info [v-box
                   :children [[:p.info-heading "Choose your taal"]
                              [:p "Enter swaras in the selected taal"]]]
        dugun-info [v-box
                    :children [[:p.info-heading "Select number of notes per beat"]
                               [:p "Select one beat to have 1, 2 or 3 notes in it."]]]
        show-partname-popup (reagent/atom false)
        show-raga-popup (reagent/atom false)
        show-taal-popup (reagent/atom false)
        show-lang-popup (reagent/atom false)
        notes-per-beat (reagent/atom 1)]
    (fn []
      (let [sbp (vec (repeat 21 (reagent/atom false)))
            speed-switch-fn (fn[i] {:disp-fn #(reset! notes-per-beat i)
                                    :state #(= i @notes-per-beat)})
            lang @(subscribe [::subs/lang])]
        [v-box
         :gap      "0.5vh"
         :children [[h-box
                     :justify :between
                     :class "first-bar"
                     :align :center
                     :children [[h-box
                                 :children
                                 [(box-button
                                   (let [lang @(subscribe [::subs/lang])]
                                     (if (= :hindi lang) "S R G" "सा रे ग"))
                                   {:disp-fn
                                    #(do (dispatch [::events/toggle-lang] ))
                                    :state (constantly false)})]]
                                [h-box
                                 :align :center
                                 :children
                                 [[h-box
                                   :children
                                   [(box-button
                                     (let [taals (get-in lang-labels [lang :tala-labels])
                                           taal @(subscribe [::subs/taal])]
                                       (taals taal))
                                     {:disp-fn
                                      #(do (reset! show-taal-popup (not @show-taal-popup)))
                                      :state #(true? @show-taal-popup)})
                                    [info-button :info taal-info]]]
                                  ]]
                                [h-box :align :center
                                 :min-width "15vw"
                                 :children
                                 [(box-button
                                   (let [ragas (get-in lang-labels [lang :raga-labels])
                                         raga @(subscribe [::subs/raga])]
                                     (ragas raga))
                                   {:disp-fn
                                    #(do (reset! show-raga-popup (not @show-raga-popup)))
                                    :state #(true? @show-raga-popup)})
                                  [info-button :info raga-info]]]
                                [h-box
                                 :align :center
                                 :children [[h-box
                                             :children [(box-button "1" (speed-switch-fn 1))
                                                        (box-button "2" (speed-switch-fn 2))
                                                        (box-button "3" (speed-switch-fn 3))]]
                                            [info-button :info dugun-info]]]]]
                    (let [swaras-3oct (swar36 @(subscribe [::subs/raga]))
                          but-style {:width (let [iw (.-innerWidth js/window)]
                                              (if (> iw 200)
                                                200 "50vw"))}]
                      [v-box
                       :gap "0.5vh"
                       :class "middle-buttons"
                       :children
                       (into
                        (let [raga-id (subscribe [::subs/raga])]
                          [[h-box
                            :gap      "0.5vw"
                            :children (mapv (partial mk-button
                                                     {:border-top
                                                      "5px solid black"}
                                                     notes-per-beat)
                                            (swaras-3oct 2))]
                           [h-box
                            :gap      "0.5vw"
                            :children (mapv (partial mk-button
                                                     notes-per-beat)
                                            (swaras-3oct 1))]
                           [h-box
                            :gap      "0.5vw"
                            :children (mapv (partial mk-button
                                                     {:border-bottom "5px solid black"}
                                                     notes-per-beat)
                                            (swaras-3oct 0))]])
                        [[h-box
                          :gap      "0.5vw"
                          :style {:flex-flow "row wrap"}
                          :class "last-bar"
                          :children [(mk-button notes-per-beat {:note [:madhyam :-]})
                                     (mk-button notes-per-beat {:note [:madhyam :a]})
                                     (butn2 "⌫" #(dispatch [::events/delete-single-swara]))
                                     (butn2 "💾"
                                            #(do
                                               ;;write the current part
                                               ;;(dispatch [:append-part])
                                               (download-link nil)))]]
                         (when @show-taal-popup
                           (let [ta (:tala-labels (lang-labels @(subscribe [::subs/lang])))
                                 taal-labels (mapv (fn[[a b]] {:id a  :label b}) ta)

                                 box-fn (fn[{:keys [id label]}]
                                          [button
                                           :label label
                                           :style but-style
                                           :on-click
                                           #(do
                                              (dispatch [::events/set-taal id])
                                              (reset! show-taal-popup
                                                      (not @show-taal-popup)))
                                           :class "btn btn-default"])
                                 children (mapv box-fn taal-labels)]
                             [modal-panel
                              :backdrop-on-click #(reset! show-taal-popup false)
                              :child [:div {:class "popup" :style {:overflow-y :scroll
                                                                   :max-height "80vh"}}
                                      [v-box
                                       :gap "2vh"
                                       :class "body"
                                       :children
                                       [[box
                                         :align :center
                                         :child [title :level :level3
                                                 :label "Select Taal"]]
                                        [v-box
                                         :align :center
                                         :children children]]]]]))
                         (when @show-raga-popup
                           (let [raga-labels (mapv (fn[[a b]] {:id a  :label b})
                                                   (:raga-labels @(subscribe [::subs/lang-data])))

                                 box-fn (fn[{:keys [id label]}]
                                          [button
                                           :label label
                                           :style but-style
                                           :on-click
                                           #(do
                                              (dispatch [::events/set-raga id])
                                              (reset! show-raga-popup
                                                      (not @show-raga-popup)))
                                           :class "btn btn-default"])
                                 children (mapv box-fn raga-labels)]
                             [modal-panel
                              :backdrop-on-click #(reset! show-raga-popup false)
                              :child [:div {:class "popup" :style {:overflow-y :scroll
                                                                   :max-height "80vh"}}
                                      [v-box
                                       :gap "2vh"
                                       :class "body"
                                       :children
                                       [[box
                                         :align :center
                                         :child [title :level :level3
                                                 :label "Show Swaras from Raga"]]
                                        [v-box
                                         :align :center
                                         :children children]]]]]))])])]]))))

(def editor-height (reagent/atom 0))
(defn swara-display-area
  []
  (let [cursor-index (reagent/atom nil)]
    (fn []
      (let [winhgt (.-innerHeight js/window)
            myhgt (- winhgt 
                     @editor-height)]
        [:div 
         [:div
          {:class "edit-composition"
           :style {:overflow-y "scroll"
                   :max-height myhgt
                   :height myhgt
                   :min-height myhgt
                   :flex-flow "column" :flex "1 0 0px"}
           ;;this code sets the scroll bar to the bottom, so that the last type text is seen.
           :ref
           #(if (identity %)
              (let [iel (.querySelector js/document ".edit-composition")
                    padding (.-padding (js->clj (.getComputedStyle js/window iel)))
                    intpadding (int (if padding
                                      (first (.split (first (.split padding " ")) "px")) " is nil"))]
                (when (> (.-scrollHeight % ) myhgt)
                  (let [sctop (- (.-scrollHeight % ) myhgt)]
                    (set! (.-scrollTop %) sctop)))))}
          [:div {:class "com-edit"
                 ;;:ref #(when (identity %) (set-scroll-top %))
                 }
           (let [div-id "editor"
                 comp @(subscribe [::subs/composition])
                 _ (dispatch [::events/reset-note-index])
                 rect-style {:width 2 :height 30 :y 10}
                 image-map (db/image-map
                            (if (= :hindi @(subscribe [::subs/lang]))
                              "hindi" "english_SrR"))
                 draw-bhaag
                 (fn[row-index bhaag-index note-map-seq]
                   (let [r3
                         (->>
                          note-map-seq
                          (map vector (range))
                          (reduce
                           (fn[{:keys [x images] :as acc} [note-index note]]
                             (let [r2
                                   (->>
                                    note
                                    (map vector (range))
                                    (reduce
                                     (fn[{:keys [x1 images1] :as acc1} [ni i]]
                                       ;;create all notes in a single beat.
                                       (let [cur-note (:note i)
                                             note-xy-map {:row-index row-index
                                                          :bhaag-index bhaag-index
                                                          :note-index note-index
                                                          :ni ni}
                                             cursor-rect
                                             [:rect (assoc rect-style
                                                           :x (+ x1 25) :y 5
                                                           :height 50)]
                                             ith-note
                                             (if-let [ival (image-map cur-note)]
                                               [:image
                                                {:height 32 :width 32
                                                 :href ival
                                                 :on-click
                                                 (fn[i]
                                                   (do
                                                     (dispatch [::events/set-click-index
                                                                note-xy-map])))
                                                 :x x1 :y 5}]
                                               ;;- and S
                                               (do 
                                                   [:text {:x (+ 10 x1) :y 25}
                                                    (name (second cur-note))]))
                                             r3 (-> acc1
                                                    (update-in [:images1] conj ith-note)
                                                    (update-in [:x1] + 20))

                                             r3 (let [curpos @(subscribe [::subs/get-click-index])]
                                                  (if (= note-xy-map curpos)
                                                    (do (println " showing cursor at " curpos)
                                                        (update-in r3 [:images1] conj cursor-rect))
                                                    r3))]
                                         r3))
                                     {:x1 x :images1 []}))]
                               (let [img-count (count (filter #{:text :image}
                                                              (map first (:images1 r2))))
                                     r5(-> acc
                                           (update-in [:x] (constantly (:x1 r2)))
                                           (update-in [:images] into (:images1 r2)))]
                                 ;;if more than 1 note in a single beat,
                                 ;;draw the ellipse under the notes
                                 (if (> img-count 1)
                                   (update-in r5 [:images]
                                              conj
                                              [:polyline
                                               {:points
                                                (let [y0 40
                                                      sl 5]
                                                  ;;line with ends curved up
                                                  (str (+ sl x) "," y0 " "
                                                       (+ x ( * 2 sl)) "," (+ y0 sl) " "
                                                       (:x1 r2) "," (+ y0 sl) " "
                                                       (+ sl (:x1 r2)) "," y0))
                                                :stroke "black"
                                                :fill "none"}])
                                   r5))))
                           {:x 5 :images []}))
                         images (:images r3)
                         x-end (:x r3)]
                     ;;add vertical bars for bhaag
                     ;;2 bars if the avartan starts
                     {:images (if (= 0 bhaag-index)
                                (into images
                                      [[:rect (assoc rect-style :x 0)]
                                       [:rect (assoc rect-style :x 3)]])
                                (conj images [:rect (assoc rect-style :x 0)]))
                      :x x-end}))
                 ;;returns  list of lists
                 ;;each element is one avartan
                 ;;each subelement is one bhaag.
                 bfn (fn[[row-index bhaag]]
                       (->> bhaag
                            (map vector (range))
                            (mapv (fn[[indx i]]
                                    (let [{:keys [images x]} (draw-bhaag row-index indx i )]
                                      [:div {:class "bhaag-item" :style  {:max-width (+ x 20)}}
                                       (reduce conj
                                               [:svg {:xmlns "http://www.w3.org/2000/svg" }]
                                               images)])))
                            (reduce conj [:div {:class "box-row"}])))
                 b1
                 (->> comp
                      :indexed-noteseq
                      (map vector (range))
                      (mapv bfn))
                 fin (reduce conj [:div {:class "wrapper"}] b1)]
             fin)]]]))))

(defn show-editor-keyboard
  []
  [:div
   ;;editor height is 0 by default, so the canvas is first drawn too large in height
   [swara-display-area]
   [:div {:class "keyboard wow fadeInUp"
          :ref #(if (identity %)
                  (let [ch (.-offsetHeight %)] 
                    (reset! editor-height ch)))}
    [swara-buttons]]])

(defmethod routes/panels :home-panel [] [show-editor-keyboard])

(defn main-panel []
  (let [active-panel (re-frame/subscribe [::subs/active-panel])]
    [re-com/v-box
     :src      (at)
     :height   "100%"
     :children [(routes/panels @active-panel)]]))
