(ns bhatkhande-editor.subs
  (:require
   [sargam.languages :refer [lang-labels]]
   [re-frame.core :as re-frame
    :refer [reg-sub]]))

(reg-sub
 ::name
 (fn [db]
   (:name db)))

(reg-sub
 ::active-panel
 (fn [db _]
   (:active-panel db)))

(reg-sub
 ::raga
 (fn [db _]
   (:raga db)))

(reg-sub
 ::edit-props 
 (fn [db _]     
   (-> db :edit-props)))

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
 ::raga
 :<- [::edit-props]
 (fn [edit-props _]     
   (-> edit-props :raga)))

(reg-sub
 ::octave-disp
 :<- [::edit-props]
 (fn [edit-props _]
   (-> edit-props :octave-switch)))

(reg-sub
 ::swaramap
 (fn [db _]
   (get-in lang-labels [(:language db) :swara-labels])))
