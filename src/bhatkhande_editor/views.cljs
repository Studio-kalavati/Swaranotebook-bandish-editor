(ns bhatkhande-editor.views
  (:require
   [re-frame.core :as re-frame :refer [subscribe dispatch dispatch-sync]]
   [re-com.core :as re-com :refer [title
                                   hyperlink v-box h-box
                                   hyperlink-href
                                   single-dropdown
                                   checkbox
                                   line
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
   [bhatkhande-editor.subs :as subs]))

(def editor-height (reagent/atom 0))
(def cursor-y (reagent/atom 0))

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
        :else
        ["80vw" "2vw"]))

(defn swar36
  "generate a vector of size 3, one each for base middle and top octaves.
  It generates the swaras for a given raga id"
  [raga-id]
  (let [com (if (= :custom raga-id)
              @(subscribe [::subs/custom-svaras])
              (remove (varjit-svaras raga-id) mswaras))
        fnx (fn[saptak] (mapv #(assoc {} :svara (vector saptak %)) com))
        res [(fnx :mandra) (fnx :madhyam) (fnx :taar)]]
    res))

(defn zmdi-butn2
  ([icon-class on-click-fn] (zmdi-butn2 {} icon-class on-click-fn))
  ([btn-style icon-class on-click-fn]
   [box
    :size "auto"
    :align-self :stretch
    :style {:flex "1 1 0px"}
    :child [:button {:style (merge btn-style {:width "100%"})
                     :class "btn"
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
    :child [:button {:style (merge style {:width "100%" :font-weight "bold"}) :class class
                     :on-click on-click-fn}
            [:span text]]]))

(defn mk-button
  ([swaralem] (mk-button {} swaralem))
  ([style {:keys [svara] :as sh}]
   (let [msmap @(subscribe [::subs/swaramap])
         m2 (msmap (second svara))]
     (butn2 m2
            #(do
               ;;since on IOS it needs a button press to start the audio context
               (dispatch-sync [::events/play-svara svara])
               (dispatch [::events/conj-svara {:svara sh}]))
            {:style (merge style {:width "100%"})}))))

(defn zmdi-box-button
  [icon-class {:keys [disp-fn state]}]
  [box :size "1"
   :justify :center
   :align-self :center
   :style {:max-width "10vw" :height "5vh"}
   :child
   [:button {:style {:width "100%" :border-radius "5px" :border-color "white"
                     :box-shadow :none}
             :class (if (true? (state))
                      "zmdi-hc-lg btn-warning btn"
                      "zmdi-hc-lg btn-default")
             :on-click #(disp-fn)}
    [:i {:class icon-class}]]])

(defn box-button
  ([style label {:keys [disp-fn state]}]
   [button
    :label [:span label]
    :style style
    :parts {:wrapper {:style {:align-items "normal"}}}
    :class "zmdi-hc-lg"
    :on-click #(disp-fn)]))

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

(defn json-cvt
  [compdata]
  (let [nseq (:noteseq compdata)
        res {:score {:part {:noteseq
                        (mapv (fn[{:keys [notes lyrics]}]
                                (let [iret {:notes (mapv (fn[{:keys [svara]}]
                                                           {:svara svara}) notes)}]
                                  (if lyrics
                                    (assoc iret :lyrics lyrics)
                                    iret)))
                              nseq)
                        :taal (:taal compdata)}
                 :version "2025-25-01"}}]
    res))

(defn download-link
  [compdata title]
  (let [json (.stringify js/JSON (clj->js (json-cvt compdata)))
        blob (js/Blob. [json]  #js {"type" "octet/stream"})
        url (.createObjectURL js/URL blob)
        a (.createElement js/document "a")]
    (set! (.-href a) url)
    (set! (.-download a) (str title ".json"))
    (.click a)))

(defn asjc-hbox
  ([children] (asjc-hbox {} children))
  ([style children]
   [h-box
    :style style
    :align :start
    :justify :center
    :children children]))

(defn set-keydown-rules
  []
  (dispatch
     [::rp/set-keydown-rules
      {:event-keys [
                    [[::events/keyboard-conj-svara :s] [{:keyCode 83}]]
                    [[::events/keyboard-conj-svara :-r] [{:keyCode 82 :shiftKey true}]]
                    [[::events/keyboard-conj-svara :r] [{:keyCode 82}]]
                    [[::events/keyboard-conj-svara :-g] [{:keyCode 71 :shiftKey true}]]
                    [[::events/keyboard-conj-svara :g] [{:keyCode 71}]]
                    [[::events/keyboard-conj-svara :m+] [{:keyCode 77 :shiftKey true}]]
                    [[::events/keyboard-conj-svara :m] [{:keyCode 77}]]
                    [[::events/keyboard-conj-svara :p] [{:keyCode 80}]]
                    [[::events/keyboard-conj-svara :-d][{:keyCode 68 :shiftKey true}]]
                    [[::events/keyboard-conj-svara :d] [{:keyCode 68}]]
                    [[::events/keyboard-conj-svara :-n] [{:keyCode 78 :shiftKey true}]]
                    [[::events/keyboard-conj-svara :n] [{:keyCode 78}]]
                    [[::events/keyboard-conj-svara :a] [{:keyCode 65}]]
                    [[::events/keyboard-conj-svara :-] [{:keyCode 189}]]

                    ;;notes per beat
                    [[::events/notes-per-beat 1] [{:keyCode 49}]]
                    [[::events/notes-per-beat 2] [{:keyCode 50}]]
                    [[::events/notes-per-beat 3] [{:keyCode 51}]]
                    [[::events/notes-per-beat 4] [{:keyCode 52}]]
                    ;;note-octave
                    [[::events/inc-octave] [{:keyCode 85}] [{:keyCode 85 :shiftKey true}]];;u
                    [[::events/dec-octave] [{:keyCode 76}] [{:keyCode 76 :shiftKey true}]];;l

                    ;;select
                    [[::events/clear-highlight] [{:keyCode 27}]];esc
                    [[::events/select :left] [{:keyCode 37 :shiftKey true}]];;select leftwards
                    [[::events/select :right] [{:keyCode 39 :shiftKey true}]];;select leftwards

                    ;;copy
                    [[::events/copy-to-clipboard] [{:keyCode 67 :ctrlKey true}]];;c copy highlighted
                    [[::events/paste-from-clipboard] [{:keyCode 86 :ctrlKey true}]];; v copy highlighted
                    [[::events/cut-to-clipboard] [{:keyCode 88 :ctrlKey true}]];; x copy highlighted

                    ;;navigation
                    [[::events/move-cursor :left] [{:keyCode 37}]];;<-
                    [[::events/move-cursor :right] [{:keyCode 39}]];;<-

                    [[::events/delete-single-swara] [{:keyCode 8}]] ;;backspace
                    ]
       }]))

(defn reset-keydown-rules
  []
  (dispatch
     [::rp/set-keydown-rules
      {:event-keys []
       :clear-keys []}]))

(defn swara-buttons
  []
  (let [show-raga-popup (reagent/atom false)
        show-select-svaras-popup (reagent/atom false)
        show-taal-popup (reagent/atom false)
        show-login-popup? (reagent/atom false)
        show-settings-popup? (reagent/atom false)
        show-title-popup? (reagent/atom false)
        show-keyboard-help? (reagent/atom false)
        show-file-popup? (reagent/atom false)
        newsletter-signup? (reagent/atom true)
        show-lyrics? (reagent/atom @(subscribe [::subs/show-lyrics?]))
        newline-on-avartan? (reagent/atom @(subscribe [::subs/newline-on-avartan?]))
        title-val (reagent/atom "")
        svaras-on @(subscribe [::subs/custom-svaras])
        font-size (reagent/atom @(subscribe [::subs/font-size]))
        selected-pitch (reagent/atom (:id (first (filter #(= (:sample %) @(subscribe [::subs/pitch])) pitch-options-list))))
        ;;if unset, all shuddha svaras
        default-custom-svaras
        (if svaras-on
          (mapv #(let [ss (into (sorted-set) svaras-on)]
                   (if (ss %) true false))
                (take 12 us/i-note-seq))
          [true false true false true true false true
           false true false true])
        octave-notes-list (mapv (fn[_ istate] (reagent/atom istate))
                                (range 12)
                                ;;all shuddha svaras true
                                default-custom-svaras)
        menu-btn [box
                  :size "auto"
                  :align-self :stretch
                  :style {:flex "1 1 0px" }
                  :child [:button
                          {:style {:width "100%"
                                   ;; :background-color "coral"
                                   }
                           :class "btn btn-lg"
                           :on-click
                           #(do
                              (dispatch [::events/set-active-panel :menu-panel]))}
                          [:i {:class "zmdi zmdi-menu zmdi-hc-2x"}]]]
        transparent-style {:background-color :transparent}
        file-btn (zmdi-butn2 transparent-style
                             "zmdi zmdi-file-text zmdi-hc-lg"
                             #(reset! show-file-popup? true))]
    (set-keydown-rules)
    (fn []
      (let [speed-switch-fn (fn[i] {:disp-fn #(dispatch [::events/notes-per-beat i])
                                    :state #(= i (or @(subscribe [::subs/notes-per-beat]) 1))})
            rag-box-style {:align-items "normal" :background-color "unset"}
            but-style {:width (let [iw (.-innerWidth js/window)]
                                (if (> iw 200)
                                  200 "50vw"))}
            logged-in? @(subscribe [::subs/user])
            save-btn
            (if @(subscribe [::subs/save-possible?])
              (zmdi-butn2 transparent-style "zmdi zmdi-floppy zmdi-hc-lg"
                          #(dispatch [::events/upsert-comp]))
              (zmdi-butn2 transparent-style "zmdi zmdi-cloud-upload zmdi-hc-lg"
                          #(if logged-in?
                             (reset! show-title-popup? true)
                             (reset! show-login-popup? true))))
            lang-btn
            (box-button
             rag-box-style
             (let [lang @(subscribe [::subs/lang])]
               (cond
                 (= :hindi lang)
                 "সা  রে"
                 (= :bangla lang)
                 "S R"
                 (= :english lang)
                 "सा रे"))
             {:disp-fn
              #(do (dispatch [::events/toggle-lang] ))
              :state (constantly false)})
            taal-btn
            (box-button rag-box-style "Tal"
                        {:disp-fn
                         #(do (reset! show-taal-popup (not @show-taal-popup)))
                         :state #(true? @show-taal-popup)})
            raga-btn
            (box-button rag-box-style "Rag"
                        {:disp-fn
                         #(do (reset! show-select-svaras-popup
                                      (not @show-select-svaras-popup)))
                         :state #(true? @show-select-svaras-popup)})
            onebeat-btn (zmdi-box-button
                         "zmdi zmdi-collection-item-1"
                         (speed-switch-fn 1))
            twobeat-btn (zmdi-box-button
                         "zmdi zmdi-collection-item-2"
                         (speed-switch-fn 2))
            threebeat-btn (zmdi-box-button
                           "zmdi zmdi-collection-item-3"
                           (speed-switch-fn 3))
            fourbeat-btn (zmdi-box-button
                          "zmdi zmdi-collection-item-4"
                          (speed-switch-fn 4))
            settings-btn (zmdi-box-button
                          "zmdi zmdi-settings zmdi-hc-lg"
                          {:disp-fn #(do (reset! show-settings-popup? true))
                           :state (constantly false)})
            play-btn (zmdi-box-button
                      "zmdi zmdi-hc-lg zmdi-play-circle"
                      {:disp-fn
                       #(let []
                          (dispatch [::events/set-mode :play])
                          (reset-keydown-rules))
                       :state (constantly false)})
            swaras-3oct (swar36 @(subscribe [::subs/raga]))]
        [:div
         {:ref #(when (identity %)
                  (let [ch (.-offsetHeight %)]
                    (reset! editor-height ch)))}
         [v-box
          :gap      "0.5vh"
          :children [(when (= :ask-hw-kbd @(subscribe [::subs/onscreen-keyboard]))
                       [modal-panel
                        :child
                        [:div {:style {:min-width "min(80vw,400px)"}}
                         [v-box
                          :gap      "5vh"
                          :class "body"
                          :align :center
                          :justify :center
                          :children
                          [[title :label "Hiding onscreen keyboard. Click the ⌨ icon to use the onscreen keyboard." :level :level3]
                           (asjc-hbox
                            [[button
                              :label "  OK  "
                              :style {:width "100px"}
                              :class "btn-hc-lg btn-primary "
                              :on-click #(dispatch [::events/hide-onscreen-keyboard])]
                             [gap :size "5vw"]])
                           [gap :size "2vh"]]]]])
                     (when @show-keyboard-help?
                       (let [bfn (fn[text kys]
                                   (asjc-hbox {:width "550px"}
                                              [[box :style {:width "250px"}
                                                :child [:p text]]
                                               [box :style {:width "250px"}
                                                :child [:p kys]]]))]
                         [modal-panel
                          :backdrop-on-click #(reset! show-keyboard-help? false)
                          :style {:style {:min-width "min(80vw,600px)"}}
                          :child
                          [v-box
                           :gap      "0.5vh"
                           :class "body"
                           :style {:max-height "90vh"
                                   :overflow-y :scroll}
                           :children
                           [(asjc-hbox {:width "550px"}
                                       [[box :style {:width "250px"}
                                         :child [:b "To type"]]
                                        [box :style {:width "250px"}
                                         :child [:b "Use keystroke"]]])
                            (bfn "Shuddha Svaras" "s, r, g, m, p, d, n")
                            (bfn "Komal Svaras" "Shift+r, Shift+g, Shift+d, Shift+n")
                            (bfn "Tivra Madhyam" "Shift+m")
                            (bfn "Avagraha (S)" "a")
                            (bfn "Vishram" "-")
                            (bfn "(Shift to) higher octave" "u/U")
                            (bfn "(Shift to) lower octave" "l/L")
                            (bfn "1 note per beat" "1")
                            (bfn "2 notes per beat (dugun)" "2")
                            (bfn "3 notes per beat (tigun)" "3")
                            (bfn "4 notes per beat (chaugun)" "4")
                            (bfn "Move cursor left" "<- (left)")
                            (bfn "Move cursor right" "-> (right)")
                            (bfn "Delete backward" "Backspace")
                            (bfn "Select (to copy)" "Shift + ->, Shift + <-")
                            (bfn "Copy" "Ctrl + c")
                            (bfn "Paste" "Ctrl + v")
                            (bfn "Cut" "Ctrl + x")
                            (bfn "Deselect" "Esc")
                            [gap :size "2vh"]
                            [box
                             :align :center
                             :child
                             [button
                              :label "  OK  "
                              :style {:width "100px"}
                              :class "btn-hc-lg btn-primary "
                              :on-click #(do (reset! show-keyboard-help? false))]]
                            [gap :size "2vh"]]]
                          ]))
                     (when @show-file-popup?
                       [modal-panel
                        :backdrop-on-click #(reset! show-file-popup? false)
                        :child
                        [:div {:style {:min-width "min(80vw,400px)"}}
                         [v-box
                          :gap      "0.5vh"
                          :class "body"
                          :children
                          [[gap :size "2vh"]
                           (asjc-hbox
                            [[title :label "Save PDF" :level :level3]
                             [gap :size "20px"]
                             [:button {:class "btn btn-lg" :on-click #(do (.print js/window))}
                              [:i {:class "zmdi zmdi-print zmdi-hc-lg"}]]])
                           [gap :size "4vh"]
                           (asjc-hbox
                            [[title :label "Save JSON" :level :level3]
                             [gap :size "20px"]
                             (let [comp @(subscribe [::subs/composition])
                                   bpm @(subscribe [::subs/bpm])
                                   pitch @(subscribe [::subs/pitch])
                                   ctitle @(subscribe [::subs/comp-title])]
                               [:button {:class "btn btn-lg" :download "something.json"
                                         :on-click #(download-link comp (or ctitle "composition"))}
                                [:i {:class "zmdi zmdi-download zmdi-hc-lg"}]])])
                           [gap :size "4vh"]
                           (asjc-hbox
                            [[title :label "Import composition" :level :level3]
                             [gap :size "20px"]
                             [:div
                              [:label ""
                               [:input {:type "file"
                                        :on-change
                                        #(let [file (-> % .-target .-files (aget 0))]
                                           (when file
                                             (do
                                               (reset! show-file-popup? false)
                                               (dispatch [::events/clear-url-path])
                                               (.pushState (.-history js/window)
                                                           #js {} ""
                                                           (str (.-origin (.-location js/window)) "/app"))
                                               (dispatch [::events/import-comp-json file]))))}]]]])
                           [gap :size "4vh"]
                           [box
                            :align :center
                            :child
                            [button
                             :label "  OK  "
                             :style {:width "100px"}
                             :class "btn-hc-lg btn-primary "
                             :on-click #(do (reset! show-file-popup? false))]]
                           [gap :size "2vh"]]]]])
                     (when @show-settings-popup?
                       [modal-panel
                        :backdrop-on-click #(reset! show-settings-popup? false)
                        :child
                        [:div {:style {:min-width "min(80vw,400px)"}}
                         [v-box
                          :gap      "0.5vh"
                          :class "body"
                          :children
                          [[gap :size "2vh"]
                           (asjc-hbox
                            [[checkbox
                              :model show-lyrics?
                              :style {:width "auto" :height "20px" }
                              :on-change
                              #(let [nval (not @show-lyrics?)]
                                 (reset! show-lyrics? nval)
                                 (dispatch [::events/show-lyrics? nval]))]
                             [gap :size "20px"]
                             [title :label "Show Lyrics?"
                              :level :level3]])
                           (asjc-hbox
                            [[checkbox
                              :model newline-on-avartan?
                              :style {:width "auto" :height "20px" }
                              :on-change
                              #(let [nval (not @newline-on-avartan?)]
                                 (reset! newline-on-avartan? nval)
                                 (dispatch [::events/newline-on-avartan? nval]))]
                             [gap :size "20px"]
                             [title :label "Newline on each Avartan?"
                              :level :level3]])
                           [gap :size "50px"]
                           [v-box
                            :align :center
                            :children
                            [[slider :model font-size
                              :min 24
                              :max 40
                              :step 4
                              :style {:align-self :center}
                              :width "max(25vw,150px)"
                              :on-change #(do (reset! font-size %)
                                              (dispatch [::events/set-font-size %]))]
                             [title :label
                              (str "Zoom: " (* 100 (/ (/ (- @font-size 24) 4) 4)) "%") :level :level3]]]
                           [gap :size "50px"]
                           [v-box
                            :align :center
                            :children
                            [[h-box :children
                              [[title :level :level3 :label "Change pitch to: "]
                               [single-dropdown
                                :choices pitch-options-list
                                :model selected-pitch
                                :width "100px"
                                :on-change
                                (fn[x]
                                  (reset! selected-pitch x)
                                  (dispatch
                                   [::events/init-note-buffers
                                    (item-for-id @selected-pitch pitch-options-list)]))]]]]]
                           [gap :size "50px"]
                           [box
                            :align :center
                            :child
                            [button
                             :label "  OK  "
                             :style {:width "100px"}
                             :class "btn-hc-lg btn-primary "
                             :on-click #(do (reset! show-settings-popup? false))]]
                           [gap :size "2vh"]]]]])
                     (when @show-title-popup?
                       [modal-panel
                        :backdrop-on-click #(reset! show-title-popup? false)
                        :child [:div {:class "popup"
                                      :style {:overflow-y :scroll
                                              :max-height "80vh"}}
                                [v-box
                                 :gap "2vh"
                                 :class "body"
                                 :align :center
                                 :children
                                 [[box
                                   :align :center
                                   :child [title :level :level2 :label "Title of the Bandish"]]
                                  [gap :size "2vh"]
                                  [box :align :center
                                   :child
                                   [input-text
                                    :src (at)
                                    :model            title-val
                                    :style {:font-size "large" :width "100%"
                                            :justify-content "center"
                                            :text-align "center"}
                                    :on-change        #(reset! title-val %)]]
                                  [button
                                   :label " Save "
                                   :class "btn-hc-lg btn-primary "
                                   :on-click #(let [tv (cstring/replace @title-val
                                                                        #" " "-")]
                                                (reset! show-title-popup? false)
                                                (dispatch [::events/upload-new-comp tv]))]]]]])
                     (when @show-login-popup?
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
                                   :child
                                   [title :level :level2
                                    :label "Login to save Bandish"]]
                                  [gap :size "3vh"]
                                  [button
                                   :style {:min-height "min(10vh,100px)"
                                           :min-width "max(20vw,200px)"}
                                   :label "Google + "
                                   :class "btn-hc-lg btn-primary btn-danger"
                                   :on-click
                                   #(do (reset! show-login-popup? false)
                                        (when @newsletter-signup?
                                          (.setItem (.-sessionStorage js/window)
                                                    "newsletter-subscribe?"
                                                    (str @newsletter-signup?)))
                                        (dispatch [::events/google-sign-in-fx]))]

                                  [gap :size "2vh"]
                                  [box
                                   :child
                                   [checkbox
                                    :style {:width "50px"}
                                    :model newsletter-signup?
                                    :label "Subscribe to newsletter"
                                    :on-change
                                    #(reset! newsletter-signup?
                                             (not @newsletter-signup?))]]]]]])
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
                     (when @show-select-svaras-popup
                       (let [box-fn (fn[model-atom lab]
                                      [h-box
                                       :style  {:width "100px"}
                                       :align :center
                                       :justify :center
                                       :children
                                       [[checkbox
                                         :model model-atom
                                         :label-style (if @model-atom  {:color "#f83600"})
                                         :on-change
                                         #(do
                                            (reset! model-atom (not @model-atom)))
                                         :style {:width "auto"
                                                 :height "20px" }
                                         ]
                                        [gap :size "20px"]
                                        [title :label lab :level :level2
                                         :style {:color (if @model-atom
                                                          "#f83600" "#000000")}]]])
                             children (mapv box-fn octave-notes-list
                                            ["S" "r" "R" "g" "G" "m" "M" "P"
                                             "d" "D" "n" "N"])]
                         [modal-panel
                          :backdrop-on-click #(reset! show-select-svaras-popup false)
                          :child [:div {:class "popup" :style {:overflow-y :scroll
                                                               :max-height "80vh"}}
                                  [v-box
                                   :gap "2vh"
                                   :class "body"
                                   :children
                                   [[box
                                     :align :center
                                     :child [title :level :level3
                                             :label "Select Svaras"]]
                                    (asjc-hbox {:min-width "100px" :flex-flow "row wrap"}
                                               children)
                                    [v-box
                                     :align :center
                                     :justify :center
                                     :children
                                     [[button
                                       :label "OK"
                                       :style (assoc but-style
                                                     :background-color "#f83600")
                                       :on-click
                                       #(let [iargs (->>
                                                     (map vector (take 12 us/i-note-seq)
                                                          (map deref octave-notes-list))
                                                     (filter (fn[[a b]] (when b a)))
                                                     (map first))]
                                          (dispatch [::events/set-custom-svaras iargs])
                                          (reset! show-select-svaras-popup
                                                  (not @show-select-svaras-popup)))
                                       :class "btn btn-default"]
                                      [gap :size "2vh"]
                                      [button
                                       :label "Select Raga"
                                       ;;:style but-style
                                       :on-click
                                       #(do
                                          (reset! show-select-svaras-popup
                                                  (not @show-select-svaras-popup))
                                          (reset! show-raga-popup
                                                  (not @show-raga-popup)))
                                       :class "btn btn-default"]
                                      ]]]]]]))
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
                                     :children children]
                                    [box
                                     :align :center
                                     :child
                                     [button
                                      :label "Cancel"
                                      :on-click
                                      #(do
                                         (reset! show-raga-popup
                                                 (not @show-raga-popup)))
                                      :class "btn btn-default"]]]]]]))
                     (when-let [{:keys [score-part-index avartan-index bhaag-index] :as cmap}
                                @(subscribe [::subs/show-lyrics-popup])]
                       (let [text-val
                             @(subscribe [::subs/get-sahitya cmap])
                             tval (reagent/atom text-val)]
                         [modal-panel
                          :backdrop-on-click #(dispatch [::events/hide-lyrics-popup])
                          :child [:div {:class "popup"
                                        :style {:overflow-y :scroll
                                                :max-height "80vh"}}
                                  [v-box
                                   :gap "2vh"
                                   :class "body"
                                   :align :center
                                   :children
                                   [
                                    [title :level :level2 :label "Lyrics"]
                                    [title :level :level4 :label
                                     "Put a comma between beats. E.g. Ae,ri,aa,li"]
                                    [gap :size "2vh"]
                                    [box :align :center
                                     :child
                                     [input-text
                                      :src (at)
                                      :model            tval
                                      :style {:font-size "large" :min-width "200px" :width "100%"}
                                      ;;debug height
                                      :on-change
                                      #(do
                                         (reset! tval %))]]
                                    [asjc-hbox
                                     [[button :label " OK "
                                       :class "btn-lg btn btn-default"
                                       :on-click
                                       #(do
                                          (dispatch [::events/conj-sahitya
                                                     (assoc cmap :text-val @tval)])
                                          (dispatch [::events/hide-lyrics-popup]))]
                                      [gap :size "2vh"]
                                      [button :label " Next "
                                       :class "btn-lg btn btn-default"
                                       :on-click
                                       #(do
                                          (dispatch [::events/conj-sahitya
                                                     {:text-val @tval
                                                      :bhaag-index bhaag-index
                                                      :avartan-index avartan-index}])
                                          (dispatch [::events/next-bhaag-lyrics-popup
                                                     {:avartan-index avartan-index
                                                      :bhaag-index bhaag-index}]))]]]]]]]))
                     (if (= :hide @(subscribe [::subs/onscreen-keyboard]))
                       [v-box
                        :children
                        [[h-box
                          :justify :end
                          :align :center
                          :style {:background "floralwhite"}
                          :children
                          [[box :child
                            [:p "Octave: "
                             [:b (cstring/capitalize
                                  (name @(subscribe[::subs/note-octave])))]]]
                           [gap :size "2vw"]
                           [button :label "View Keyboard Help "
                            :class "btn-lg btn btn-secondary "
                            :on-click
                            #(reset! show-keyboard-help? true)]]]
                         [h-box
                          :gap      "0.5vw"
                          :style {:flex-flow "row wrap"}
                          :class "last-bar"
                          :children [menu-btn
                                     (zmdi-butn2 transparent-style
                                                 "zmdi zmdi-keyboard zmdi-hc-lg"
                                                 #(dispatch [::events/show-onscreen-keyboard]))
                                     file-btn save-btn
                                     lang-btn
                                     taal-btn
                                     onebeat-btn twobeat-btn threebeat-btn fourbeat-btn
                                     settings-btn play-btn]]]]
                       [v-box :children [[h-box
                                          :justify :between
                                          :class "first-bar"
                                          :align :center
                                          :children [lang-btn
                                                     taal-btn
                                                     raga-btn
                                                     onebeat-btn twobeat-btn threebeat-btn fourbeat-btn
                                                     settings-btn play-btn]]
                                         [v-box
                                          :gap "0.5vh"
                                          :class "middle-buttons"
                                          :children
                                          (into
                                           [[h-box
                                             :gap      "0.5vw"
                                             :children (mapv (partial mk-button
                                                                      {:border-top
                                                                       "5px solid black"})
                                                             (swaras-3oct 2))]
                                            [h-box
                                             :gap      "0.5vw"
                                             :children (mapv (partial mk-button)
                                                             (swaras-3oct 1))]
                                            [h-box
                                             :gap      "0.5vw"
                                             :children (mapv (partial mk-button
                                                                      {:border-bottom "5px solid black"})
                                                             (swaras-3oct 0))]]
                                           [[h-box
                                             :gap      "0.5vw"
                                             :style {:flex-flow "row wrap"}
                                             :class "last-bar"
                                             :children [menu-btn file-btn save-btn
                                                        (mk-button {:svara [:madhyam :-]})
                                                        (mk-button {:svara [:madhyam :a]})
                                                        (zmdi-butn2 "zmdi zmdi-tag-close zmdi-hc-lg"
                                                                    #(dispatch [::events/delete-single-swara]))]]])]]])]]]))))

(defn swara-display-area
  []
  (let [edit-part-index (reagent/atom nil)
        delete-confirm (reagent/atom false)]
    (fn []
      (let [winhgt (.-innerHeight js/window)
            myhgt (- winhgt
                     @editor-height)
            show-lyrics? @(subscribe [::subs/show-lyrics?])
            font-size (reagent/atom @(subscribe [::subs/font-size]))
            newline-on-avartan? @(subscribe [::subs/newline-on-avartan?])
            hidden-parts @(subscribe [::subs/hidden-parts])
            play-mode? (= :play @(subscribe [::subs/mode]))
            _ @(subscribe [::subs/onscreen-keyboard])]
        [:div
         [:div
          {:class "edit-composition"
           :style {:overflow-y "scroll"
                   ;;:max-height myhgt
                   :height myhgt
                   :min-height myhgt
                   :flex-flow "column" :flex "1 0 0px"}
           ;;this code sets the scroll bar to the bottom, so that the last type text is seen.
           :ref
           #(when (identity %)
              (dispatch [::events/set-music-notes-element %])
              (if play-mode?
                (set! (.-scrollTop %) 0)
                (when (> (.-scrollHeight % ) myhgt)
                  (let [sctop  (- (.-scrollHeight % ) myhgt)
                        curpos (+ @cursor-y (.-scrollTop %))]
                    #_(println " setting sctop to  "  sctop " cursor-y " @cursor-y " myhgt " myhgt
                               " sctop "(.-scrollHeight % ) " cur sroll top "(.-scrollTop %))
                    (when (> curpos sctop )
                      (set! (.-scrollTop %) sctop))))))}
          (when @delete-confirm
            [modal-panel
           :backdrop-on-click #(reset! delete-confirm false)
             :child
             [:div {:style {:min-width "min(80vw,400px)"
                            :display "flex"
                            :justify-content "center"}}
              [v-box
               :gap "4vh"
               :align :center
               :children
               (if (> (->> @(subscribe [::subs/composition]) :score-parts count) 1)
                 [[title :level :level2 :label "Delete Part?"]
                  [h-box :gap "2vw" :children
                   [[button :label "Cancel" :class "btn btn-default"
                     :on-click (fn[_] (reset! delete-confirm nil))]
                    [button :label "Delete" :class "btn btn-danger"
                     :on-click (fn[_]
                                 (do
                                   (dispatch [::events/delete-part @delete-confirm])
                                   (reset! delete-confirm nil)))]]]]
                 [[title :level :level2 :label "The first part cannot be deleted"]
                  [h-box :gap "2vw" :children
                   [[button :label "OK" :class "btn btn-info"
                     :on-click (fn[_] (reset! delete-confirm nil))]]]])]]])

          [:div {:class "com-edit"}
           (let
               [comp @(subscribe [::subs/composition])
                editing @(subscribe [::subs/currently-editing])
                rect-style {:width 2 :height @font-size :y (int (* 0.3 @font-size))}
                image-map (db/image-map
                           (let [ilang @(subscribe [::subs/lang])]
                             (if (or (= :bangla ilang) (= :hindi ilang))
                               (name ilang)
                               "english_SrR")))
                draw-bhaag
                (fn[score-part-index
                    avartan-index
                    bhaag-index
                    note-map-seq]
                  (let [cursor-map {:score-part-index score-part-index
                                    :avartan-index avartan-index
                                    :bhaag-index bhaag-index}

                        sahitya (->> (get-in comp [:indexed-noteseq score-part-index avartan-index bhaag-index]))
                        r3
                        (->>
                         note-map-seq
                         (map vector (range))
                         (reduce
                          (fn[{:keys [x _] :as acc} [note-index note]]
                            (let [;;this is the flat noteseq index.
                                  ;;example: at position 11, we find
                                  ;;11  --  {:notes [{:svara [:madhyam :m+], :npb 3} {:svara [:madhyam :g], :npb 3} {:svara [:madhyam :r], :npb 3}]}
                                  ;;which can have multiple notes in it.
                                  nseq-index
                                  (db/get-noteseq-index
                                   cursor-map
                                   (:taal comp))
                                  r2
                                  (->>
                                   note
                                   :notes
                                   (map vector (range))
                                   (reduce
                                    (fn[{:keys [x1 _] :as acc1}
                                        [nsi {:keys [svara]}]]
                                      ;;create all notes in a single beat.
                                      (let [note-xy-map (assoc cursor-map :nsi nsi :note-index note-index)
                                            ith-note
                                            (if-let [ival (image-map svara)]
                                              [:image
                                               {:height @font-size :width @font-size
                                                :href ival
                                                :class
                                                (let [highlight-v
                                                      ;;don't match note sub index because only the
                                                      ;;first (or single) note is present in the highlight vector
                                                      (map #(dissoc % :nsi)
                                                           @(subscribe [::subs/highlighted-pos-set]))]
                                                  (if (some #(= (dissoc note-xy-map :nsi) %)
                                                            highlight-v)
                                                    "highlight-color" ""))
                                                :on-click
                                                (fn[i]
                                                  (reset! cursor-y (.-pageY i))
                                                  (dispatch [::events/currently-editing :svaras])
                                                  (dispatch [::events/set-click-index
                                                             ;;for multi-note, always show on the first
                                                             (assoc note-xy-map
                                                                    :nsi 0)]))
                                                :x x1 :y 5}]
                                              ;;- and S
                                              [:text {:x (+ (int (* 0.3 @font-size)) x1)
                                                      :y (cond
                                                           (> @font-size 32) 32
                                                           (< @font-size 24) 24
                                                           :else @font-size)
                                                      :on-click
                                                      (fn[_]
                                                        (dispatch [::events/currently-editing :svaras])
                                                        (dispatch [::events/set-click-index
                                                                   note-xy-map]))}
                                               (name (second svara))])
                                            r3 (-> acc1
                                                   (update-in [:images1] conj ith-note)
                                                   (update-in [:x1] + (int (* 0.7 @font-size))))

                                            ;;if edit mode, a single cursor
                                            ;;if play mode, add all rects
                                            r3
                                            (if play-mode?
                                              (update-in
                                               r3 [:images1] conj
                                               (let [phi @(subscribe [::subs/play-head-position])]
                                                 [:rect
                                                  {:width (int (* 0.6 @font-size)) :height @font-size
                                                   :fill "#f83600"
                                                   :fill-opacity 0
                                                   ;;todo-uncomment
                                                   :ref
                                                   #(when (identity %)
                                                      (let [opa "fill-opacity:0"
                                                            opac (str opa
                                                                      (if (= phi
                                                                             (assoc cursor-map
                                                                                    :note-index note-index
                                                                                    :nsi nsi))
                                                                        ".5" ""))]
                                                        (set! (.-style %) opac)
                                                        (dispatch [::events/register-elem
                                                                   nseq-index note-xy-map %])))
                                                   :x (+ x1 (int (* 0.2 @font-size)))
                                                   :y (int (* 0.2 @font-size))}]))
                                              (let [curpos @(subscribe [::subs/get-click-index])]
                                                (if (= note-xy-map curpos)
                                                  (do (println " curpos " curpos )
                                                      (update-in
                                                       r3 [:images1] conj
                                                       [:rect
                                                        (assoc rect-style
                                                               :x (+ x1 5)
                                                               :y (if (= editing :sahitya) 25 5)
                                                               :height (int (* 1.3 @font-size))
                                                               :ref #(when (identity %)
                                                                       ;;when moving, don't blink
                                                                       ;;after its stationary start blinking
                                                                       (js/setTimeout
                                                                        (fn[]
                                                                          (.add (.-classList %)
                                                                                "blinking-cursor"))
                                                                        1000)))]))
                                                  r3)))
                                            #_r3 #_(if-let [sah (get sah-list note-index)]
                                                     (if (= nsi 0)
                                                       (let [r4
                                                             (update-in r3
                                                                        [:images1]
                                                                        conj
                                                                        [:text
                                                                         {:x (+ x1 (int (* 0.3 @font-size)))
                                                                          :y (int (* 1.7 @font-size))
                                                                          :style {:font-size (* 0.5 @font-size)}
                                                                          :on-click
                                                                          (fn[i]
                                                                            (reset! cursor-y (.-pageY i))
                                                                            (dispatch [::events/currently-editing :sahitya])
                                                                            (dispatch [::events/set-click-index
                                                                                       (assoc note-xy-map :nsi 0)]))}
                                                                         sah])]
                                                         (if (> 2 (count sah))
                                                           r4
                                                           (-> r4
                                                               (update-in [:x1] + (int (* (* 0.3 (count sah))
                                                                                          (* 0.7 @font-size)))))))
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
                                                    (let [y0 (int (* 1 @font-size))
                                                          sl (int (* 0.2 @font-size))]
                                                      ;;line with ends curved up
                                                      (str (+ sl x) "," y0 " "
                                                           (+ x ( * 2 sl)) "," (+ y0 sl) " "
                                                           (:x1 r2) "," (+ y0 sl) " "
                                                           (+ sl (:x1 r2)) "," y0))
                                                    :stroke "black"
                                                    :fill "none"}])
                                       r5)
                                  ;;add sam-khaali
                                  r7 (if (= 0 note-index)
                                       (update-in r6
                                                  [:images] conj
                                                  [:text {:x (int (* 0.5 @font-size))
                                                          :y  (if (or show-lyrics? sahitya)
                                                                (int (* 2.3 @font-size))
                                                                (int (* 1.6 @font-size)))
                                                          :style {:font-size (* 0.5 @font-size)}}
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
                        (:images r3)
                        #_(if show-lyrics?
                            (-> (:images r3)
                                (into (let [tv @(subscribe [::subs/get-sahitya
                                                            [avartan-index bhaag-index]])]
                                        [[:defs
                                          [:linearGradient {:id "sahitya-fill"}
                                           [:stop {:offset "0%" :stop-color "gray"}]
                                           [:stop {:offset "100%" :stop-color "lightgray"}]]]
                                         [:rect
                                          {:x (int (* 0.2 @font-size)) :y (int (* 1.3 @font-size))
                                           :width (:x r3) :height (int (* 0.6 @font-size))
                                           :rx "5"
                                           :style
                                           (let [m {:font-size "15px"}]
                                             (merge m
                                                    (if sahitya
                                                      {:fill "transparent"}
                                                      {:fill "url(#sahitya-fill)"})))
                                           :on-click
                                           (fn[_]
                                             (dispatch [::events/currently-editing :lyrics]))}]])))
                            (:images r3))
                        x-end (:x r3)]
                    ;;add vertical bars for bhaag
                    ;;2 bars if the avartan starts
                    {:images
                     (let [pfn (fn[i] (int (* i @font-size)))
                           line-style {:y2 (+ @font-size (pfn 0.3))
                                       :y1 (pfn 0.3)
                                       :stroke-width (pfn 0.05)
                                       :stroke :black}]
                       (if (= 0 bhaag-index)
                         (into images
                               [[:line (assoc line-style :x1 1 :x2 1)]
                                [:line (assoc line-style :x1 (pfn 0.1) :x2 (pfn 0.1))]])
                         (conj images [:line (assoc line-style :x1 1 :x2 1)])))
                     :x x-end}))
                ;;returns  list of lists
                ;;each element is one avartan
                ;;each subelement is one bhaag.
                disp-score-part
                (fn[score-part-index score-part]
                  (let [score-res
                        (->> score-part
                             (map-indexed
                              (fn[avartan-index row]
                                (let [res0
                                      (->> (map-indexed
                                            (fn[bhaag-index bhaag]
                                              (let [{:keys [images x]}
                                                    (draw-bhaag score-part-index avartan-index bhaag-index bhaag)
                                                    cursor-map {:score-part-index score-part-index
                                                                :avartan-index avartan-index
                                                                :bhaag-index bhaag-index}
                                                    sah-list (clojure.string/join ","
                                                                                  (get-sahitya comp cursor-map))

                                                    res [:div {:class "bhaag-item" :style
                                                               (merge
                                                                {:max-width (+ x (int (* @font-size 0.7))) }
                                                                {:max-height
                                                                 (int (* (if @(subscribe [::subs/show-lyrics?]) 2.5 2)
                                                                         @font-size))})}
                                                         (reduce conj
                                                                 [:svg {:xmlns "http://www.w3.org/2000/svg"
                                                                        :width (+ x (int (* @font-size 0.6)))}]
                                                                 images)
                                                         (let [ topsize (str (* 1.1 @font-size) "px")]
                                                           [input-text :model sah-list
                                                            :width "50px"
                                                            :class "overlay-text"
                                                            :style {:top topsize :font-size (* 0.8 @font-size)}
                                                            :on-change (fn[x]
                                                                         (let [new-sahitya
                                                                               (clojure.string/split x #",")]
                                                                           (dispatch [::events/conj-sahitya
                                                                                      (assoc cursor-map :text-val new-sahitya)])))])]]
                                                res))
                                            row)
                                           vec)]
                                  res0)))
                             vec)
                        part-header
                        [h-box
                         :style {:justify-content "space-between"}
                         :children
                         [
                          (let [edited-part-name (reagent/atom
                                                  (get-in comp [:score-parts score-part-index :part-title]))]
                            (if (= @edit-part-index score-part-index)
                              [h-box
                               :gap "1vw"
                               :align :center :justify :center
                               :children
                               [
                                [gap :size "1vw"]
                                [input-text :model edited-part-name
                                 :on-change (fn[x] (reset! edited-part-name x))]
                                [md-icon-button :md-icon-name "zmdi zmdi-check-circle"
                                 :on-click (fn[] (do
                                                   (dispatch [::events/update-part-title score-part-index
                                                                @edited-part-name])
                                                   (reset! edit-part-index nil)))]]]
                              [h-box
                               :gap "1vw"
                               :align :center :justify :center
                               :children
                               [
                                [gap :size "1vw"]
                                [title :level :level3
                                 :label (get-in comp [:score-parts score-part-index :part-title])]
                                [md-icon-button :md-icon-name "zmdi zmdi-edit"
                                 :on-click (fn[] (reset! edit-part-index score-part-index))]]]))
                          [h-box
                           :gap "1vw"
                           :align :center :justify :center
                           :children
                           [(if (hidden-parts score-part-index)
                              [md-icon-button :md-icon-name "zmdi zmdi-chevron-down zmdi-hc-lg"
                               :on-click (fn[] (dispatch [::events/unhide-part score-part-index]))]
                              [md-icon-button :md-icon-name "zmdi zmdi-chevron-up zmdi-hc-lg"
                               :on-click (fn[] (dispatch [::events/hide-part score-part-index]))])
                            [md-icon-button :md-icon-name "zmdi zmdi-delete zmdi-hc-lg"
                             :on-click (fn[]
                                         (reset! delete-confirm score-part-index))]
                            [gap :size "0.5vw"]]]]]
                        part-footer
                        [v-box :align :center :children
                         [[md-icon-button :md-icon-name "zmdi zmdi-plus-circle-o"
                           :on-click (fn[] (dispatch [::events/insert-empty-part "abcd"]))]]]
                        score-fin
                        (let [rows (if newline-on-avartan?
                                     (mapv #(reduce conj [:div {:class "box-row"}] %) score-res)
                                     (reduce conj [:div {:class "box-row"}] (reduce into score-res)))
                              score-ret
                              [:div {:class "part-wrapper part-border"}
                               part-header
                               (when-not (hidden-parts score-part-index) rows)
                               [gap :size "1vh"]]]
                          [:div {:class "wrapper"} score-ret part-footer])]
                    score-fin))
                fin
                (->> comp
                     :indexed-noteseq
                     (map-indexed disp-score-part)
                     vec
                     (reduce conj [:div {:class "score-parts"}]))]
             fin)]]]))))



(defn play-keyboard-footer
  []
  (let [bpm (reagent/atom @(subscribe [::subs/bpm]))
        beat-mode (reagent/atom @(subscribe [::subs/beat-mode]))
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
           :child [:div {:style {:min-width "min(80vw,400px)"
                                                :display "flex"
                                                :justify-content "center"}}
                   [v-box
                    :gap "2vh"
                    :class "body"
                    :justify :center
                    :children
                    [
                     [v-box
                      :align :center
                      :children
                      [[slider :model bpm
                        :min 60
                        :max 300
                        :step 15
                        :width "40vw"
                        :disabled? @(subscribe [::subs/playing?])
                        :on-change #(do (reset! bpm %)
                                        (dispatch [::events/set-bpm %]))]
                       [h-box
                        :gap "1vh"
                        :children
                        [[title :level :level2
                          :label "BPM"]
                         [title :level :level2
                          :label @bpm]]]]]
                     [gap :size "2vh"]
                     [v-box
                      :align :center
                      :justify :center
                      :style {:min-width "200px"}
                      :children
                      [(doall (for [c ["metronome" "tabla"]]
                                ^{:key c}
                                [radio-button :src (at)
                                 :label       c
                                 :value       (let [k (keyword c)] k)
                                 :label-style {:width "200px"}
                                 :model       beat-mode
                                 :on-change   #(do
                                                   (reset! beat-mode %)
                                                   (dispatch [::events/beat-mode
                                                              (keyword @beat-mode)]))]))]]
                     [gap :size "2vh"]
                     [v-box
                      :align :center
                      :justify :center
                      :children [[checkbox
                                  :model tanpura?
                                  :label-style {:width "200px"}
                                  :label "Play Tanpura?"
                                  :on-change
                                  #(let [nval (not @tanpura?)]
                                     (reset! tanpura? nval)
                                     (dispatch [::events/tanpura? nval]))]]]
                     [h-box :children
                      [(zmdi-butn2 "zmdi zmdi-close zmdi-hc-2x"
                                   #(reset! show-settings? false))]]]]]])
        (let [back-play-settings-butns (if @(subscribe [::subs/playing?])
                                         [(zmdi-butn2
                                           "zmdi zmdi-pause-circle zmdi-hc-4x"
                                           #(do (dispatch [::events/pause])))]
                                         [(zmdi-butn2 "zmdi zmdi-arrow-left zmdi-hc-2x"
                                                      #(do (dispatch [::events/set-mode :edit])))
                                          (zmdi-butn2
                                           "zmdi zmdi-play-circle zmdi-hc-4x"
                                           #(do (dispatch [::events/play])))
                                          (zmdi-butn2 "zmdi zmdi-settings zmdi-hc-2x"
                                                      #(do (reset! show-settings? true)))])
              mobile? @(subscribe[::bp/mobile?])
              slider-play-head [slider :model (or @(subscribe [::subs/avartan-to-play-from]) 0)
                                :max (dec @(subscribe [::subs/max-num-avartans]))
                                :style {:align-self :center :height (if mobile? "3vh" "")}
                                :width (if mobile? "80vw" "max(25vw,150px)")
                                :on-change #(do
                                              (println " sph " (or % 0))
                                              (dispatch [::events/set-play-position (or % 0)]))]]
          [v-box :children
           [(when-not @(subscribe [::subs/playing?])
              slider-play-head)
            [h-box
             :gap      "0.5vh"
             :children back-play-settings-butns]]])]])))

(defn menu
  []
(fn []
      (let [icon-style {:padding "10px 0px 10px 10vw" :color "black" :font-size "x-large"}
            logged-in? @(subscribe [::subs/user])
            text-style {:padding "10px 5vw 10px 1px"}
            bbox
            [v-box
             :justify :center
             :children
             [(when logged-in?
                (let [ifn #(do
                             (dispatch [::events/list-files]))]
                  [h-box :justify :between :align :center :children
                   [[box
                     :style icon-style
                     :size "1"
                     :child
                     [md-icon-button :md-icon-name "zmdi zmdi-view-list-alt zmdi-hc-lg"
                      :on-click ifn]]
                    [gap :size "20px"]
                    [box
                     :style text-style
                     :size "10" :child
                     [hyperlink :label "My Notations"
                      :style {:font-size "x-large" :color "black"}
                      :on-click ifn]]]]))
              [box :align :center
               :child [line :class "menu-line-separator"]]
              (let [ifn #(set! (.-href (.-location js/window)) "/docs/help.html")
                    ]
                [h-box :justify :between :align :center :children
                 [[box :size "1"
                   :style icon-style
                   :child
                   [md-icon-button :md-icon-name "zmdi zmdi-help zmdi-hc-lg"
                    :on-click ifn]]
                  [gap :size "20px"]
                  [box
                   :style text-style
                   :size "10" :child
                   [hyperlink :label "Help Center"
                    :style {:font-size "x-large" :color "black"}
                    :on-click ifn]]]])
              [box :align :center
               :child [line :class "menu-line-separator"]
               ]
              (let [ifn #(do
                           (if logged-in?
                             (dispatch [::events/sign-out])
                             (dispatch [::events/google-sign-in-fx]))
                           (dispatch [::events/set-active-panel :home-panel]))]
                [h-box :justify :between :align :center :children
                 [[box :size "1"
                   :style icon-style
                   :child
                   [md-icon-button :md-icon-name "zmdi zmdi-key zmdi-hc-lg"
                    :on-click ifn]]
                  [gap :size "20px"]
                  [box
                   :style text-style
                   :size "10" :child
                   [hyperlink :label (if logged-in? "Log out" "Log in")
                    :style {:font-size "x-large" :color "black"}
                    :on-click ifn]]]])
              [box :align :center
               :child
               [line :class "menu-line-separator"]
               ]
              (let [ifn #(do
                           (dispatch [::events/clear-url-path])
                           (dispatch [::events/refresh-comp db/init-comp])
                           (.pushState (.-history js/window)
                                       #js {} ""
                                       (str (.-origin (.-location js/window)) "/app")))]
                [h-box :justify :between :align :center :children
                 [[box :size "1"
                   :style icon-style
                   :child
                   [md-icon-button :md-icon-name "zmdi zmdi-file-plus zmdi-hc-lg"
                    :on-click ifn]]
                  [gap :size "20px"]
                  [box
                   :style text-style
                   :size "10" :child
                   [hyperlink :label "New Bandish"
                    :style {:font-size "x-large" :color "black"}
                    :on-click ifn]]]])
              [box :align :center
               :child
               [line :class "menu-line-separator"]
               ]
              (let [ifn #(do (dispatch [::events/set-active-panel :home-panel]))]
                [h-box :justify :between :align :center :children
                 [[box :size "1"
                   :style icon-style
                   :child
                   [md-icon-button :md-icon-name "zmdi zmdi-chevron-left zmdi-hc-lg"
                    :on-click ifn]]
                  [gap :size "20px"]
                  [box
                   :style text-style
                   :size "10" :child
                   [hyperlink :label "Back"
                    :style {:font-size "x-large" :color "black"}
                    :on-click ifn]]]])]]]
        [:div
         {:class "edit-composition"
          :style {:min-height "100vh"}}
         bbox])))

(defn help-panel
  []
  [v-box :children
   [
    [v-box :children
     [[title :label "abcd" :level :level2]
      [:img
       {:src
        "https://user-images.githubusercontent.com/89076/225162591-fef23263-0fa0-4262-9809-47fba749be38.gif"}]]]
    [box
     :style {:padding "10px 5vw 10px 1px"}
     :size "10" :child
     [hyperlink :label "Back"
      :style {:font-size "x-large" :color "black"}
      :on-click #(dispatch [::events/set-active-panel :home-panel])]]]])

(defn list-comps
  []
  (let [delete-comp (reagent/atom nil)
        share-comp (reagent/atom nil)]
    (fn []
      (let [bands @(subscribe [::subs/my-bandishes])
            bbox (mapv (fn[i0]
                         (let [i (second (clojure.string/split i0 #"/"))
                               uuid (-> @(subscribe [::subs/user]) :uid)
                               id (clojure.string/join
                                   "" (rest (clojure.string/split i0 #"/")))
                               iurl (db/get-long-url (str uuid "/" id))
                               title-label (->> (clojure.string/split i #"-")
                                                rest
                                                (clojure.string/join "-"))]
                           [v-box
                            :children
                            [[h-box
                              :justify :between
                              :align :center
                              :children
                              [[hyperlink-href
                                :style {:padding "10px 0px 10px 10vw"
                                        :color "black"
                                        :font-size "x-large"}
                                :label title-label
                                :href iurl]
                               [h-box
                                :style {:padding "10px 5vw 10px 1px"}
                                :gap "4vw"
                                :children
                                [[box :size "1"
                                  :child
                                  [md-icon-button :md-icon-name "zmdi zmdi-share"
                                   :on-click (fn[_]
                                               (if (.-share js/navigator)
                                                 (-> (.share js/navigator
                                                             #js
                                                             {"title" title-label
                                                              "text"
                                                              " Check out this notation \n"
                                                              "url"
                                                              (db/get-long-url i0)})
                                                     (.then (fn[i] (println " shared " i)))
                                                     (.catch (fn[i]
                                                               (println " share error " i))))
                                                 ;;put in a popup if share is not enabled
                                                 (reset! share-comp i0)))]]
                                 [box :size "1" :child [md-icon-button :md-icon-name "zmdi zmdi-delete "
                                                        :on-click (fn[_] (reset! delete-comp i0))]]]]]]
                             [box
                              :align :center
                              :child
                              [line :class "menu-line-separator"]
                              ]
                             ]]))
                       bands)
            bbox (conj bbox
                       (let [ifn #(do (dispatch [::events/set-active-panel :home-panel]))
                             icon-style {:padding "10px 0px 10px 10vw" :color "black" :font-size "x-large"}
                             text-style {:padding "10px 5vw 10px 1px"}
                             ]
                         [h-box :justify :between :align :center :children
                          [[box :size "1"
                            :style icon-style
                            :child
                            [md-icon-button :md-icon-name "zmdi zmdi-chevron-left zmdi-hc-lg"
                             :on-click ifn]]
                           [gap :size "20px"]
                           [box
                            :style text-style
                            :size "10" :child
                            [hyperlink :label "Back"
                             :style {:font-size "x-large" :color "black"}
                             :on-click ifn]]]]))
            confirm-panel [modal-panel
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
                                      [title :level :level3 :label "Delete? "]]

                                     [h-box :children
                                      [[button :label "Cancel"
                                        :on-click (fn[_] (reset! delete-comp nil))]
                                       [gap :size "5vw"]
                                       [button :label "Delete"
                                        :class "btn-danger"
                                        :on-click (fn[_]
                                                    (dispatch [::events/delete-comp @delete-comp])
                                                    (reset! delete-comp nil))]]]]]]]
            share-panel [modal-panel
                         :backdrop-on-click #(reset! share-comp nil)
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
                                    (db/get-long-url @share-comp)]
                                   [box :align :center
                                    :child
                                    [button
                                     :label "  OK  "
                                     :style {:width "100px"}
                                     :class "btn-hc-lg btn-primary "
                                     :on-click #(do (reset! share-comp nil))]]]]]]]

        [:div
         {:class "edit-composition"
          :style {:min-height "100vh"}}
         [v-box :children
          (cond @delete-comp
                (conj bbox confirm-panel)
                @share-comp
                (conj bbox share-panel)
                :else bbox)]]))))

(defn show-editor
  []
  [:div
   [swara-display-area]
   [:div {:class "keyboard wow fadeInUp"
          :ref #(when (identity %)
                  (let [ch (.-offsetHeight %)]
                    (reset! editor-height ch)))}
    (let [istate @(subscribe [::subs/mode])]
      (if (= :play istate)
        [play-keyboard-footer]
        [swara-buttons]))]])

(defn wait-for
  [msg]
  [:div
   [modal-panel
    :backdrop-color "floralwhite"
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
               [title :level :level3 :label msg]]
              [throbber :color "coral" :size :large]]]]]])


(defmethod routes/panels :load-panel []
  (wait-for "Loading notations"))

(defmethod routes/panels :load-sounds-panel []
  (wait-for "Loading Santoor and Tabla sounds"))

(defmethod routes/panels :home-panel [] [show-editor])

(defmethod routes/panels :list-comps-panel [] [list-comps])

(defmethod routes/panels :help-panel [] [help-panel])

(defmethod routes/panels :menu-panel [] [menu])

(defmethod routes/panels :wait-for-save-completion []
  (wait-for "Saving notation"))

(defmethod routes/panels :import-error-panel []
  [:div
   [modal-panel
    :backdrop-color "floralwhite"
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
               [title :level :level3 :label "Invalid format "]]
              [box :align :center
               :child
               [button
                :label "  OK  "
                :style {:width "100px"}
                :class "btn-hc-lg btn-primary "
                :on-click #(dispatch [::events/set-active-panel :home-panel])]]]]]]])

(defmethod routes/panels :wait-for-loading-comps []
  (wait-for "Loading notations"))

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
               :max-width "80vw"}
              {})
     :children [(routes/panels @active-panel)]]))
