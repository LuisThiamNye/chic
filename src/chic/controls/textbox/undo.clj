(ns chic.controls.textbox.undo
  (:require
   [chic.controls.textbox.helper :as hpr]
   [chic.debug]))

(defn significant-change? [^long nchars ^long t]
  (cond
    (<= nchars 0) false
    (< 2000 nchars) true
    :else
    (let [timeth (unchecked-multiply 3400 (Math/exp (unchecked-multiply -0.0027 nchars)))]
      (<= timeth t))))

(defn unsaved-undos? [state]
  (let [uci (:undo-commit-idx state)]
    (or (< uci 0)
        (not (identical? (:rope state) (nth (nth (:undo-commits state) uci) 0))))))

(defn save-undo-commit* [{:keys [undo-commits undo-commit-idx] :as state}]
  (-> state
      (update :undo-commit-idx inc)
      (assoc :last-undo-commit-time (System/currentTimeMillis))
      (assoc :undo-commits
             (conj (cond-> undo-commits
                     (< undo-commit-idx (dec (count undo-commits)))
                     (subvec 0 (inc undo-commit-idx)))
                   [(:rope state) (:cursor-idx state) (:select-idx state)]))))

(defn apply-undo-commit* [state uci]
  (let [[rope' cursor-idx' select-idx'] (nth (:undo-commits state) uci)]
    (-> state
        (assoc :rope rope')
        (assoc :cursor-idx cursor-idx')
        (assoc :select-idx select-idx')
        (assoc :undo-commit-idx uci)
        (assoc :last-undo-commit-time (System/currentTimeMillis))
        hpr/recalculate-derived*)))

(defn perform-undo* [{:keys [] :as state}]
  (let [uci0 (:undo-commit-idx state)]
    (if (< uci0 0)
      state
      (let [;; ensure latest commit matches live text
            state (cond-> state (and (== uci0 (dec (count (:undo-commits state))))
                                     (unsaved-undos? state))
                          (save-undo-commit*))
            undo-commits (:undo-commits state)
            uci2 (dec (:undo-commit-idx state))]
        (if (< uci2 0)
          state
          (apply-undo-commit* state uci2))))))

(defn perform-redo* [{:keys [undo-commits] :as state}]
  (let [uci0 (:undo-commit-idx state)]
    (if (<= (dec (count undo-commits)) uci0)
      state
      (let [uci2 (inc (:undo-commit-idx state))]
        (apply-undo-commit* state uci2)))))

(defn maybe-save-undo [*state]
  (let [state @*state
        uci (:undo-commit-idx state)
        save-undo? (or (< uci 0)
                       (let [undo-commits (:undo-commits state)
                             undo-commit (nth undo-commits uci)
                             rope1 (:rope state)
                             nchardiff (if (identical? rope1 (nth undo-commit 0)) 0 1)
                             t (System/currentTimeMillis)]
                         (significant-change? nchardiff (- t (:last-undo-commit-time state)))))]
    (when save-undo?
      ;; (chic.debug/println-main "saving undo commit...")
      (vswap! *state save-undo-commit*))))
