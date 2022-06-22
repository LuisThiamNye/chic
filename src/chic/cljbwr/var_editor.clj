(ns chic.cljbwr.var-editor
  (:require
   [io.github.humbleui.paint :as huipaint]
   [chic.ui.error :as ui.error]
   [chic.controls.button :as button]
   [chic.ui :as cui]
   [chic.ui.event :as uievt]
   [chic.text-editor :as text-editor]
   [chic.ui.layout :as cuilay]
   [clojure.string :as str]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.skija Paint]))

(def x 4)

(defn var-panel [vr]
  (cuilay/valign
   0
   (cuilay/halign
    0
    (button/lone-button
     {:on-click (fn [_]
                  (ns-unmap (find-ns (symbol (namespace (symbol vr))))
                            (symbol (name (symbol vr)))))
      :label "Undef"}))))
