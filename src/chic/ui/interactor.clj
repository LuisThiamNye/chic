(ns chic.ui.interactor)

(defn standard-button [{:keys [no-hover? on-click]}]
  [[:cursor-rect
    {:rect :component}]]
  (fn [ctx rect]))

(defn interactable [interactors geometry])
