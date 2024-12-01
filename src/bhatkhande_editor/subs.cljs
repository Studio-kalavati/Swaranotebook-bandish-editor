(ns bhatkhande-editor.subs
  (:require
   [sargam.languages :refer [lang-labels]]
   [bhatkhande-editor.db :as db]
   [re-frame.core :as re-frame :refer [reg-sub]]))

(reg-sub
 ::lang
 (fn [db]
   (-> db :props :lang)))

(reg-sub
 ::pitch
 (fn [db]
   (let [ pitch (-> db :props :pitch)]
     pitch)))

(reg-sub
 ::font-size
 (fn [db]
   (-> db :dispinfo :font-size)))

(reg-sub
 ::lang-data
 :<- [::lang]
 (fn [lang _]
   (lang-labels lang)))

(reg-sub
 ::active-panel
 (fn [db _]
   (:active-panel db)))

(reg-sub
 ::props
 (fn [db _]
   (-> db :props)))

(reg-sub
 ::raga
 :<- [::props]
 (fn [props _]     
   (-> props :raga)))

(reg-sub
 ::x-switch
 :<- [::props]
 (fn [props _]     
   (-> props :x-switch)))

(reg-sub
 ::error-message
 :<- [::props]
 (fn [props _]     
   (-> props :error-message)))

(reg-sub
 ::mode
 :<- [::props]
 (fn [props _]
   (-> props :mode)))

(reg-sub
 ::swaramap
 :<- [::lang]
 (fn [lang _]
   (get-in lang-labels [lang :swara-labels])))

(reg-sub
 ::composition
 (fn [db _]
   (:composition db)))

(reg-sub
 ::taal
 :<- [::composition]
 (fn [comp _]
   (-> comp :taal)))

(reg-sub
 ::comp-title
 :<- [::composition]
 (fn [comp _]
   (-> comp :title)))

(reg-sub
 ::save-possible?
 (fn [db [_ _]]
   (and (= (-> db :props :path) (-> db :user :uid))
        (not (nil? (-> db :props :id))))))

(reg-sub
 ::get-click-index
 (fn [db [_ _]]
   (get-in db [:props :cursor-pos])))

(reg-sub
 ::show-text-popup
 (fn [db [_ _]]
   (get-in db [:props :show-text-popup])))

(reg-sub
 ::get-sahitya
 (fn [db [_ [row-index bhaag-index]]]
   (let [indx (db/get-noteseq-index {:row-index row-index
                                     :bhaag-index bhaag-index
                                     :note-index 0}
                                    (get-in db [:composition :taal]))]
     (-> db (get-in [:composition :noteseq indx :lyrics])))))

(reg-sub
 ::get-note-pos
 (fn [db [_ [bhaag-row-index bhaag-index note-index]]]
   (get-in db [:props :note-pos bhaag-row-index bhaag-index note-index])))

(reg-sub
 ::user
 (fn [db [_ _]]
   (:user db)))

#_(reg-sub
 ::bandish-url
 (fn [db [_ _]]
   (:bandish-url db)))

(reg-sub
 ::audio-context
 (fn [db [_ _]]
   (:audio-context db)))

(reg-sub
 ::sample-buffers
 (fn [db [_ _]]
   (:sample-buffers db)))

(reg-sub
 ::show-lyrics?
 :<- [::props]
 (fn [props [_ _]]
   (:show-lyrics props)))

(reg-sub
 ::custom-svaras
 :<- [::props]
 (fn [props [_ _]]
   (:custom-svaras props)))

(reg-sub
 ::newline-on-avartan?
 :<- [::props]
 (fn [props [_ _]]
   (:newline-on-avartan? props)))

(reg-sub
 ::playing?
 (fn [db [_ _]]
   (= :start (:play-state db))))

(reg-sub
 ::bpm
 :<- [::props]
 (fn [props [_ _]]
   (:bpm props)))

(reg-sub
 ::beat-mode
 :<- [::props]
 (fn [props [_ _]]
   (:beat-mode props)))

(reg-sub
 ::tanpura?
 :<- [::props]
 (fn [props [_ _]]
   (:tanpura? props)))

(reg-sub
 ::my-bandishes
 (fn [db [_ _]]
   (:my-bandishes db)))

(reg-sub
 ::bhaag-to-play-from
 (fn [db [_ _]]
   (:nth-bhaag-to-play-from db)))

(reg-sub
 ::play-head-position
 (fn [db [_ _]]
   (:play-head-position db)))

;;number of bhaags to play in play mode
(reg-sub
 ::max-num-bhaags
 (fn [db [_ _]]
   (count (:bhaag-first-note db))))
