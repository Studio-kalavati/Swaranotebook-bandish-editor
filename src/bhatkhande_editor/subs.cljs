(ns bhatkhande-editor.subs
  (:require
   [sargam.languages :refer [lang-labels]]
   [bhatkhande-editor.db :as db]
   [re-frame.core :as re-frame :refer [reg-sub]]))

(reg-sub
 ::lang
 (fn [db]
   (if (-> db :edit-props :language-en?)
     :english :hindi)))

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
 ::edit-props
 (fn [db _]
   (-> db :edit-props)))

(reg-sub
 ::raga
 :<- [::edit-props]
 (fn [edit-props _]     
   (-> edit-props :raga)))

(reg-sub
 ::x-switch
 :<- [::edit-props]
 (fn [edit-props _]     
   (-> edit-props :x-switch)))

(reg-sub
 ::error-message
 :<- [::edit-props]
 (fn [edit-props _]     
   (-> edit-props :error-message)))

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
 ::get-click-index
 (fn [db [_ _]]
   (get-in db [:edit-props :cursor-pos])))

(reg-sub
 ::show-text-popup
 (fn [db [_ _]]
   (get-in db [:edit-props :show-text-popup])))

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
   (println "sub "[bhaag-row-index bhaag-index note-index])
   (get-in db [:edit-props :note-pos bhaag-row-index bhaag-index note-index])))

(reg-sub
 ::user
 (fn [db [_ _]]
   (:user db)))

(reg-sub
 ::bandish-url
 (fn [db [_ _]]
   (:bandish-url db)))

(reg-sub
 ::show-keyboard?
 (fn [db [_ _]]
   (get-in db [:edit-props :show-keyboard?])))

(reg-sub
 ::share-url
 (fn [db [_ _]]
   (db/get-long-url (:bandish-url db))))

(reg-sub
 ::audio-context
 (fn [db [_ _]]
   (:audio-context db)))

(reg-sub
 ::santoor-buffers
 (fn [db [_ _]]
   (:santoor-buffers db)))

(reg-sub
 ::show-lyrics?
 (fn [db [_ _]]
   (:show-lyrics? db)))

(reg-sub
 ::newline-on-avartan?
 (fn [db [_ _]]
   (:newline-on-avartan? db)))

(reg-sub
 ::playing?
 (fn [db [_ _]]
   (or (= :start (:play-state db)))))
