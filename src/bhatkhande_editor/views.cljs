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
                                   p
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
        fnx (fn[saptak] (mapv #(assoc {} :shruti (vector saptak %)) com))
        res [(fnx :mandra) (fnx :madhyam) (fnx :taar)]]
    res))

(defn zmdi-butn2
  ([icon-class on-click-fn]
   [box
    :size "auto"
    :align-self :stretch
    :style {:flex "1 1 0px"}
    :child [:button {:style {:width "100%"}
                     :class "btn btn-lg"
                     :on-click on-click-fn}
            [:i {:class icon-class}]]]))

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
  ([style notes-per-beat {:keys [shruti] :as sh}]
   (let [msmap @(subscribe [::subs/swaramap])]
     (butn2 (msmap (second shruti))
            #(do
               ;;since on IOS it needs a input to start the audio context
               (dispatch-sync [::events/play-svara shruti])
               (dispatch [::events/conj-svara
                          {:svara sh :notes-per-beat @notes-per-beat}]))
            {:style (merge style {:width "100%"})}))))

(defn zmdi-box-button
  [icon-class {:keys [disp-fn state]}]
  [box :size "1"
   :justify :center
   :align-self :stretch
   :style {:max-width "10vw"}
   :child
   [:button {:style {:width "100%" :border-radius "5px"}
             :class (if (true? (state))
                      "zmdi-hc-2x btn-danger"
                      "zmdi-hc-lg btn-default")
             :on-click #(disp-fn)}
    [:i {:class icon-class}]]])

(defn box-button
  ([label ifn]
   (box-button {:padding "10px"} label ifn))
  ([style label {:keys [disp-fn state]}]
   [box :size "1"
    :justify :center
    :align-self :stretch
    :child
    [button
     :label [:span label]
     :style style
     :class (if (true? (state)) "btn-lg swarabuttons btn-sm btn-danger"
                "btn-lg swarabuttons btn-sm btn-default")
     :on-click #(disp-fn)]]))

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
        show-login-popup? (reagent/atom false)
        show-share-popup? (reagent/atom false)
        show-settings-popup? (reagent/atom false)
        show-title-popup? (reagent/atom false)
        newsletter-signup? (reagent/atom true)
        show-lyrics? (reagent/atom @(subscribe [::subs/show-lyrics?]))
        newline-on-avartan? (reagent/atom @(subscribe [::subs/newline-on-avartan?]))
        title-val (reagent/atom "")
        notes-per-beat (reagent/atom 1)]
    (fn []
      (let [sbp (vec (repeat 21 (reagent/atom false)))
            speed-switch-fn (fn[i] {:disp-fn #(reset! notes-per-beat i)
                                    :state #(= i @notes-per-beat)})
            lang @(subscribe [::subs/lang])
            rag-box-style {:padding "10px 4px 10px 4px"}
            ]
        [v-box
         :gap      "0.5vh"
         :children [[h-box
                     :justify :between
                     :class "first-bar"
                     :align :center
                     :children [[h-box
                                 :children
                                 [(box-button
                                   rag-box-style
                                   (let [lang @(subscribe [::subs/lang])]
                                     (if (= :hindi lang)
                                       ;;"S R G" "सा रे ग"
                                       "S R" "सा रे"))
                                   {:disp-fn
                                    #(do (dispatch [::events/toggle-lang] ))
                                    :state (constantly false)})]]
                                [h-box
                                 :align :center
                                 :children
                                 [[h-box
                                   :children
                                   [(box-button
                                     rag-box-style
                                     (let [taals (get-in lang-labels [lang :tala-labels])
                                           taal @(subscribe [::subs/taal])]
                                       ;;(taals taal)
                                       "Tal")
                                     {:disp-fn
                                      #(do (reset! show-taal-popup (not @show-taal-popup)))
                                      :state #(true? @show-taal-popup)})
                                    #_[info-button :info taal-info]]]
                                  ]]
                                [h-box :align :center
                                 :min-width "15vw"
                                 :children
                                 [(box-button
                                   rag-box-style
                                   (let [ragas (get-in lang-labels [lang :raga-labels])
                                         raga @(subscribe [::subs/raga])]
                                     #_(ragas raga)
                                     "Rag")
                                   {:disp-fn
                                    #(do (reset! show-raga-popup (not @show-raga-popup)))
                                    :state #(true? @show-raga-popup)})
                                  #_[info-button :info raga-info]]]
                                (zmdi-box-button
                                 "zmdi zmdi-collection-item-1"
                                 (speed-switch-fn 1))
                                (zmdi-box-button
                                 "zmdi zmdi-collection-item-2"
                                 (speed-switch-fn 2))
                                (zmdi-box-button
                                 "zmdi zmdi-collection-item-3"
                                 (speed-switch-fn 3))
                                (zmdi-box-button
                                 "zmdi zmdi-settings zmdi-hc-lg"
                                 {:disp-fn #(do (reset! show-settings-popup? true))
                                  :state (constantly false)})
                                (zmdi-box-button
                                 "zmdi zmdi-hc-lg zmdi-play-circle"
                                 {:disp-fn #(do (dispatch [::events/show-keyboard :play]))
                                  :state (constantly false)})
                                ]]
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

                        [(let [logged-in? @(subscribe [::subs/user])]
                           [h-box
                            :gap      "0.5vw"
                            :style {:flex-flow "row wrap"}
                            :class "last-bar"
                            :children [

                                       [box
                                        :size "auto"
                                        :align-self :stretch
                                        :style {:flex "1 1 0px" }
                                        :child [:button
                                                {:style {:width "100%"
                                                         :background-color "coral"}
                                                 :class "btn btn-lg"
                                                 :on-click
                                                 #(do (dispatch [::events/show-keyboard :hide]))}
                                                [:i {:class "zmdi zmdi-chevron-down zmdi-hc-lg"}]]]
                                       (zmdi-butn2 "zmdi zmdi-print zmdi-hc-lg"
                                                   #(do (.print js/window)))
                                       #_(zmdi-butn2 "zmdi zmdi-download zmdi-hc-lg"
                                                   #(let [comp @(subscribe [::subs/composition])]
                                                      (download-link
                                                       (select-keys comp [:noteseq :taal]))))
                                       (if logged-in?
                                         (zmdi-butn2 "zmdi zmdi-sign-in zmdi-hc-lg"
                                                     #(do (dispatch [::events/sign-out])))
                                         (zmdi-butn2 "zmdi zmdi-google-plus zmdi-hc-lg"
                                                     #(do (reset! show-login-popup? true))))
                                       (when-let [ctitle @(subscribe [::subs/comp-title])]
                                         (let [share-url @(subscribe [::subs/share-url])]
                                           (zmdi-butn2
                                            "zmdi zmdi-share zmdi-hc-lg"
                                            #(let []
                                               (println " share url " share-url)
                                               (if (.-share js/navigator)
                                                 (-> (.share js/navigator
                                                             #js
                                                             {"title" @title-val
                                                              "text" " Check out this bandish I wrote"
                                                              "url"
                                                              (str
                                                               (.-origin (.-location js/window))
                                                               "/view/"
                                                               share-url)})
                                                     (.then (fn[i] (println " shared")))
                                                     (.catch (fn[i]
                                                               (println " share error"))))
                                                 ;;put in a popup if share is not enabled
                                                 (do
                                                   (reset! show-share-popup? true))
                                                 )))))
                                       (when logged-in?
                                         (zmdi-butn2
                                          "zmdi zmdi-cloud-upload zmdi-hc-lg"
                                          (if-let [ctitle @(subscribe [::subs/comp-title])]
                                            #(dispatch [::events/upload-comp-json])
                                            #(reset! show-title-popup? true))))
                                       (mk-button notes-per-beat {:shruti [:madhyam :-]})
                                       (mk-button notes-per-beat {:shruti [:madhyam :a]})
                                       (butn2 "⌫" #(dispatch [::events/delete-single-swara]))]])
                         (when @show-settings-popup?
                           [modal-panel
                            :backdrop-on-click #(reset! show-settings-popup? false)
                            :child
                            [:div {:class "popup" :style {:width "85vw"}}
                             [v-box
                              :gap      "0.5vh"
                              :children
                              [(let []
                                 [v-box
                                  :gap "5vh"
                                  :style {:min-height "20vh"}
                                  :justify :center
                                  :children
                                  (let []
                                    [[gap :size "2vh"]
                                     [checkbox
                                      :model show-lyrics?
                                      :label "Show Lyrics?"
                                      :on-change
                                      #(let [nval (not @show-lyrics?)]
                                         (reset! show-lyrics? nval)
                                         (dispatch [::events/show-lyrics? nval]))]
                                     [checkbox
                                      :model newline-on-avartan?
                                      :label "Newline on each Avartan?"
                                      :on-change
                                      #(let [nval (not @newline-on-avartan?)]
                                         (reset! newline-on-avartan? nval)
                                         (dispatch [::events/newline-on-avartan? nval]))]
                                     [box
                                      :align :center
                                      :child
                                      [button
                                       :label "  OK  "
                                       :style {:width "100px"}
                                       :class "btn-hc-lg btn-primary "
                                       :on-click #(do (reset! show-settings-popup? false))]]
                                     [gap :size "2vh"]
                                     ])])]]]])
                         (when @show-share-popup?
                           [modal-panel
                              :backdrop-on-click #(reset! show-share-popup? false)
                              :child [:div {:class "popup" :style {;;:overflow :scroll
                                                                   :max-width "85vh"}}
                                      [v-box
                                       :gap "2vh"
                                       :class "body"
                                       :children
                                       [[box
                                         :align :center
                                         :child
                                         [title :level :level3
                                          :label "Copy this link to share the Bandish"]]
                                        [gap :size "3vh"]
                                        [:p {:style {:display "inline-block"
                                                     :word-wrap "break-word"}}
                                         @(subscribe [::subs/share-url])]
                                        [box :align :center
                                         :child
                                         [button
                                          :label "  OK  "
                                          :style {:width "100px"}
                                          :class "btn-hc-lg btn-primary "
                                          :on-click #(do (reset! show-share-popup? false))]]]]]])
                         (when @show-title-popup?
                           (let []
                             [modal-panel
                              :backdrop-on-click #(reset! show-title-popup? false)
                              :child [:div {:class "popup" :style {:overflow-y :scroll
                                                                   :max-height "80vh"}}
                                      [v-box
                                       :gap "2vh"
                                       :class "body"
                                       :align :center
                                       :children
                                       [[box
                                         :align :center
                                         :child [title :level :level3 :label "Bandish title"]]
                                        [gap :size "3vh"]
                                        [box :align :center
                                         :child
                                         [input-text
                                          :src (at)
                                          :model            title-val
                                          :style {:font-size "large" :width "200px" :text-align "center"}
                                          :on-change        #(reset! title-val %)]]
                                        [button
                                         :label " Save "
                                         :class "btn-hc-lg btn-primary "
                                         :on-click #(let [tv (clojure.string/replace @title-val
                                                                                     #" " "-")]

                                                      (reset! show-title-popup? false)
                                                      (dispatch [::events/upload-comp-json
                                                                 tv]))]]]]]))
                         (when @show-login-popup?
                           (let []
                             [modal-panel
                              :backdrop-on-click #(reset! show-login-popup? false)
                              :child [:div {:class "popup" :style {:overflow-y :scroll
                                                                   :max-height "80vh"}}
                                      [v-box
                                       :gap "2vh"
                                       :class "body"
                                       :align :center
                                       :children
                                       [[box
                                         :align :center
                                         :child [title :level :level3
                                                 :label "Login to save & share Bandish"]]
                                        [gap :size "3vh"]
                                        [button
                                         :label "Google + "
                                         :class "btn-hc-lg btn-primary btn-danger"
                                         :on-click #(do (reset! show-login-popup? false)
                                                        (dispatch [::events/sign-in]))]
                                        #_[checkbox
                                         :model newsletter-signup?
                                         :label "Sign up for an occasional email"
                                         :on-change #(reset! newsletter-signup? (not @newsletter-signup?))]]]]]))
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
                                         :children children]]]]]))
                         (when-let [{:keys [row-index bhaag-index]}
                                    @(subscribe [::subs/show-text-popup])]
                           (let [text-val
                                 @(subscribe [::subs/get-sahitya
                                              [row-index bhaag-index]])
                                 tval (reagent/atom text-val)]
                             [modal-panel
                              :backdrop-on-click #(dispatch [::events/hide-text-popup])
                              :child [:div {:class "popup"
                                            :style {:overflow-y :scroll
                                                    :max-height "80vh"}}
                                      [v-box
                                       :gap "2vh"
                                       :class "body"
                                       :align :center
                                       :children
                                       [[box :align :center
                                         :child
                                         [input-text
                                          :src (at)
                                          :model            tval
                                          :style {:font-size "large" :width "200px"}
                                          ;;debug height
                                          :on-change
                                          #(do
                                             (reset! tval %))]]
                                        [button :label " OK "
                                         :class "btn-lg btn btn-default"
                                         :on-click
                                         #(do
                                            (dispatch [::events/conj-sahitya
                                                         {:text-val @tval
                                                          :bhaag-index bhaag-index
                                                          :row-index row-index}])
                                            (dispatch [::events/hide-text-popup])
                                            )
                                         ]]]]]))])])]]))))

(def editor-height (reagent/atom 0))
(defn swara-display-area
  []
(fn []
      (let [winhgt (.-innerHeight js/window)
            myhgt (- winhgt
                     @editor-height)
            show-lyrics? @(subscribe [::subs/show-lyrics?])
            newline-on-avartan? @(subscribe [::subs/newline-on-avartan?])]
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
                 }
           (let
               [div-id "editor"
                 comp @(subscribe [::subs/composition])
                 _ (dispatch [::events/reset-note-index])
                 rect-style {:width 2 :height 30 :y 10}
                 image-map (db/image-map
                            (if (= :hindi @(subscribe [::subs/lang]))
                              "hindi" "english_SrR"))
                 draw-bhaag
                 (fn[row-index bhaag-index note-map-seq]
                   (let [nsindex (db/get-noteseq-index
                                  {:row-index row-index
                                   :bhaag-index bhaag-index :note-index 0}
                                  (:taal comp))
                         sahitya (get-in comp [:noteseq nsindex :lyrics])
                         sah-list (when sahitya (clojure.string/split sahitya #","))
                         r3
                         (->>
                          note-map-seq
                          (map vector (range))
                          (reduce
                           (fn[{:keys [x images] :as acc} [note-index note]]
                             (let [
                                   ;;this is the flat noteseq index.
                                   ;;example: at position 11, we find
                                   ;;11  --  {:notes [{:shruti [:madhyam :m+], :npb 3} {:shruti [:madhyam :g], :npb 3} {:shruti [:madhyam :r], :npb 3}]}
                                   ;;which can have multiple notes in it.
                                   nseq-index
                                   (db/get-noteseq-index
                                    {:row-index row-index
                                     :bhaag-index bhaag-index
                                     :note-index note-index}
                                    (:taal comp))
                                   r2
                                   (->>
                                    note
                                    :notes
                                    (map vector (range))
                                    (reduce
                                     (fn[{:keys [x1 images1] :as acc1}
                                         [nsi {:keys [shruti] :as cur-note}]]
                                       ;;create all notes in a single beat.
                                       (let [note-xy-map {:row-index row-index
                                                          :bhaag-index bhaag-index
                                                          :note-index note-index
                                                          :nsi nsi}
                                             cursor-rect
                                             (if (= :play @(subscribe [::subs/show-keyboard?]))
                                               ;;show rect that animates on playing
                                               [:rect
                                                {:width 20 :height 30
                                                 :fill "#f83600"
                                                 :fill-opacity 0
                                                 :ref #(when (identity %)
                                                         (dispatch [::events/register-elem
                                                                    nseq-index nsi %]))
                                                 :x (+ x1 5) :y 5}]
                                               ;;show cursor
                                               [:rect (assoc rect-style
                                                             :x (+ x1 5) :y 5
                                                             :height 50
                                                             :class "blinking-cursor")]
                                               )
                                             ith-note
                                             (if-let [ival (image-map shruti)]
                                               [:image
                                                {:height 32 :width 32
                                                 :href ival
                                                 :on-click
                                                 (fn[i]
                                                   (do
                                                     (dispatch [::events/set-click-index
                                                                ;;for multi-note, always show on the first
                                                                (assoc note-xy-map
                                                                       :nsi 0)])))
                                                 :x x1 :y 5}]
                                               ;;- and S
                                               (do
                                                 [:text {:x (+ 10 x1) :y 30
                                                         :on-click
                                                         (fn[i]
                                                           (dispatch [::events/set-click-index
                                                                      note-xy-map]))}
                                                  (name (second shruti))]))
                                             r3 (-> acc1
                                                    (update-in [:images1] conj ith-note)
                                                    (update-in [:x1] + 20))

                                             ;;if edit mode, a single cursor
                                             ;;if play mode, add all rects
                                             r3
                                             (if (= :play @(subscribe [::subs/show-keyboard?]))
                                               (update-in r3 [:images1] conj cursor-rect)
                                               (let [curpos @(subscribe [::subs/get-click-index])]
                                                 (if (= note-xy-map curpos)
                                                   (do
                                                     (update-in r3 [:images1] conj cursor-rect))
                                                   r3))
                                               )
                                             r3 (if-let [sah (get sah-list note-index)]
                                                  (if (= nsi 0)
                                                    (-> r3
                                                        (update-in
                                                         [:images1]
                                                         conj
                                                         [:text {:x (+ 10 x1)
                                                                 :style {:font-size "15px"}
                                                                 :y 60} sah]))
                                                    r3)
                                                  r3)]
                                         r3))
                                     {:x1 x :images1 []}))

                                   r5(-> acc
                                         (update-in [:x] (constantly (:x1 r2)))
                                         (update-in [:images] into (:images1 r2)))
                                   ;;if more than 1 note in a single beat,
                                   ;;draw the ellipse under the notes
                                   r6 (if (> (count (:notes note)) 1)
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
                                        r5)
                                   ;;add sam-khaali
                                   r7 (if (and (= 0 note-index))
                                        (update-in r6
                                                   [:images] conj
                                                   [:text {:x 14
                                                           :y  (if show-lyrics? 88 60)
                                                           :style {:font-size "15px"}}
                                                    (let [t @(subscribe [::subs/taal])
                                                          sk-index
                                                          (->> taal-def t :bhaags
                                                               (take bhaag-index)
                                                               (apply +)
                                                               inc)]
                                                      (get (-> taal-def t :sam-khaali )
                                                           sk-index))])
                                        r6)
                                   r8 (update-in
                                       r7
                                       [:images] conj)]
                               r8))
                           {:x 5 :images []}))

                         images
                         (if show-lyrics?
                           (-> (:images r3)
                             (into (let [tv @(subscribe [::subs/get-sahitya
                                                         [row-index bhaag-index]])]
                                     [
                                      [:defs
                                       [:linearGradient {:id "sahitya-fill"}
                                        [:stop {:offset "0%" :stop-color "gray"}]
                                        [:stop {:offset "100%" :stop-color "lightgray"}]]]
                                      [:rect
                                       {:x 5 :y 48
                                        :width (:x r3) :height 25
                                        :rx "5"
                                        :style
                                        (let [m {:font-size "15px"}]
                                          (merge m
                                                 (if sahitya
                                                   {:fill "transparent"}
                                                   {:fill "url(#sahitya-fill)"})))
                                        :on-click
                                        (fn[i]
                                          (dispatch [::events/show-text-popup
                                                     {:row-index row-index
                                                      :text-val (if tv tv "")
                                                      :bhaag-index bhaag-index}]))}]])))
                           (:images r3))
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
                                      [:div {:class "bhaag-item" :style
                                             (merge
                                              {:max-width (+ x 20)}
                                              (if @(subscribe [::subs/show-lyrics?])
                                                {}
                                                {:max-height "70px"}))}
                                       (reduce conj
                                               [:svg {:xmlns "http://www.w3.org/2000/svg"
                                                      }]
                                               images)])))
                            #_(reduce conj [:div {:class "box-row"}])))
                 b1
                 (->> comp
                      :indexed-noteseq
                      (map vector (range))
                      (mapv bfn))

                 fin
                 (if newline-on-avartan?
                   (->> b1
                        (mapv #(reduce conj [:div {:class "box-row"}] %))
                        (reduce conj [:div {:class "box-row"}]))
                   (->> b1
                        (reduce into [:div {:class "box-row"}])
                        (conj [:div {:class "wrapper"}])))]
             fin)]]]))
  )

(defn hidden-keyboard-footer
  []
  (fn []
    [v-box
     :gap      "0.5vh"
     :children [
                [h-box
                 :gap      "0.5vw"
                 :style {:flex-flow "row wrap"}
                 :class "last-bar"
                 :children [(zmdi-butn2 "zmdi zmdi-comment-edit zmdi-hc-lg"
                                        #(do (dispatch [::events/show-keyboard :default])))
                            (zmdi-butn2 "zmdi zmdi-file-plus zmdi-hc-lg"
                                        #(do
                                           (dispatch [::events/refresh-comp
                                                      (db/comp-decorator db/init-comp)])
                                           (.pushState (.-history js/window)
                                                       #js {} ""
                                                       (.-origin (.-location js/window)))))]]]]))

(defn play-keyboard-footer
  []
  (let [
        bpm (reagent/atom @(subscribe [::subs/bpm]))
        metronome? (reagent/atom @(subscribe [::subs/metronome?]))
        show-settings? (reagent/atom false)
        tanpura? (reagent/atom true)]
    (fn []
      [v-box
       :class "first-bar"
       :children
       [
        (when @show-settings?
          [modal-panel
           :backdrop-on-click #(reset! show-settings? false)
           :child [:div {:class "popup" :style {;;:overflow-y :scroll
                                                :width "90vw"
                                                }}
                   [v-box
                    :gap "2vh"
                    :class "body"
                    :align :center
                    :children
                    [
                     [v-box :children
                      [[slider :model bpm
                        :min 60
                        :max 180
                        :step 15
                        :width "40vw"
                        :disabled? @(subscribe [::subs/playing?])
                        :on-change #(do (reset! bpm %)
                                        (dispatch [::events/set-bpm %]))]
                       [h-box
                        :gap "1vh"
                        :children
                        [[title :level :level3
                          :label "BPM"]
                         [title :level :level3
                          :label @bpm]]]]]
                     [v-box
                      :align :center
                      :justify :center
                      :children [
                                 [checkbox
                                  :model metronome?
                                  :label "Metronome"
                                  :on-change
                                  #(let [nval (not @metronome?)]
                                     (reset! metronome? nval)
                                     (dispatch [::events/metronome? nval]))]]]
                     [v-box
                      :align :center
                      :justify :center
                      :children [
                                 [checkbox
                                  :model tanpura?
                                  :label "Play Tanpura?"
                                  :on-change
                                  #(let [nval (not @tanpura?)]
                                     (reset! tanpura? nval)
                                     (dispatch [::events/tanpura? nval]))]]]
                     [h-box :children
                      [(zmdi-butn2 "zmdi zmdi-close zmdi-hc-2x"
                                   #(reset! show-settings? false))]]]]]])
        [h-box
         :gap      "0.5vh"
         :children
         [(zmdi-butn2 "zmdi zmdi-arrow-left zmdi-hc-2x"
                      #(do (dispatch [::events/show-keyboard :default])))
          (if @(subscribe [::subs/playing?])
            (zmdi-butn2
             "zmdi zmdi-pause-circle zmdi-hc-4x"
             #(do (dispatch [::events/pause])))
            (zmdi-butn2
             "zmdi zmdi-play-circle zmdi-hc-4x"
             #(do (dispatch [::events/play]))))
          (zmdi-butn2 "zmdi zmdi-settings zmdi-hc-2x"
                      #(do (reset! show-settings? true)))]]]])))

#_(defn keyboard-more
  []
  (let [show-lyrics? (reagent/atom @(subscribe [::subs/show-lyrics?]))
        newline-on-avartan? (reagent/atom @(subscribe [::subs/newline-on-avartan?]))]
    (fn []
      [v-box
       :gap      "0.5vh"
       :children [[h-box
                   :justify :between
                   :class "first-bar"
                   :align :center
                   :children [(zmdi-butn2
                               "zmdi zmdi-arrow-left zmdi-hc-lg"
                               #(do (dispatch [::events/show-keyboard :default])))]]
                  (let []
                    [v-box
                     :gap "2vh"
                     :class "middle-buttons"
                     :style {:min-height "20vh"}
                     :align :center
                     :justify :center
                     :children
                     (into
                      (let [raga-id (subscribe [::subs/raga])]
                        [[checkbox
                          :model show-lyrics?
                          :label "Show Lyrics?"
                          :style {:padding "20px"}
                          :on-change
                          #(let [nval (not @show-lyrics?)]
                             (reset! show-lyrics? nval)
                             (dispatch [::events/show-lyrics? nval]))]
                         [checkbox
                          :model newline-on-avartan?
                          :label "Newline on each Avartan?"
                          :on-change
                          #(let [nval (not @newline-on-avartan?)]
                             (reset! newline-on-avartan? nval)
                             (dispatch [::events/newline-on-avartan? nval]))]]))])]])))

(defn show-editor
  []
  [:div
   ;;editor height is 0 by default, so the canvas is first drawn too large in height
   [swara-display-area]
   [:div {:class "keyboard wow fadeInUp"
          :ref #(if (identity %)
                  (let [ch (.-offsetHeight %)]
                    (reset! editor-height ch)))}
    (let [istate @(subscribe [::subs/show-keyboard?])]
      (cond ;;(= :more istate)
            ;;[keyboard-more]
            (= :hide istate)
            [hidden-keyboard-footer]
            (= :play istate)
            [play-keyboard-footer]
            :else [swara-buttons]))]])


(defn load-bandish
  []
  [:div
   [modal-panel
    :child [:div {:class "popup"
                  :style {:overflow-y :scroll
                          :max-height "80vh"}}
            [v-box
             :gap "2vh"
             :class "body"
             :align :center
             :children
             [[box :align :center
               :child
               [title :level :level3 :label "Loading Bandish"]]]]]]])

(defmethod routes/panels :load-panel [] [load-bandish])

(defmethod routes/panels :home-panel [] [show-editor])

(defmethod routes/panels :wait-for-save-completion []
  [:div
   [modal-panel
    :backdrop-color "#f83600"
    :child [:div {:class "popup"
                  :style {:overflow-y :scroll
                          :max-height "80vh"}}
            [v-box
             :gap "2vh"
             :class "body"
             :align :center
             :children
             [[box :align :center
               :child
               [title :level :level3 :label "Saving Bandish"]]
              [throbber :size :large]]]]]])

(defn main-panel []
  (let [active-panel (subscribe [::subs/active-panel])
        screen-height @(subscribe [::bp/screen-height])]
    [v-box
     :src      (at)
     :height   "100%"
     :style (if (and (not= :mobile @(subscribe [::bp/screen]))
                     @(subscribe [::bp/landscape?]))
              {:margin-right "auto"
               :margin-left "auto"
               :max-width (* 0.8 screen-height)}
              {})
     :children [(routes/panels @active-panel)]]))
