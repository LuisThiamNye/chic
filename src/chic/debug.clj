(ns chic.debug
  (:require
   [puget.printer :as puget]
   [puget.color.ansi])
  (:import
   (java.lang StackWalker StackWalker$Option)
   (io.github.humbleui.jwm App)))

(defonce ^java.io.OutputStreamWriter main-out *out*)

(defn println-main [& args]
  (doseq [a (interpose " " args)]
    (.write main-out (str a)))
  (.write main-out "\n")
  (.flush main-out))

(defn ^com.sun.jdi.ThreadReference vm-thread-named
  [^com.sun.jdi.VirtualMachine vm thread-name]
  (some (fn [^com.sun.jdi.ThreadReference thread]
          (when (.equals (.name thread) thread-name)
            thread))
        (.allThreads vm)))

(defn snapshot-thread [^com.sun.jdi.ThreadReference thread]
  (.suspend thread)
  (try
    (with-meta
      {:name (.name thread)
       :frames
       (with-meta
         (mapv (fn [^com.sun.jdi.StackFrame frame]
                 {:visible-variables (try (into (with-meta {} {::type.stack-frame-variables true})
                                                (.getValues frame (.visibleVariables frame)))
                                          (catch Exception e e))
                  :argument-values (try (into [] (.getArgumentValues frame))
                                        (catch Exception e e))
                  :this-object (.thisObject frame)
                  :location (.location frame)})
               (.frames thread))
         {::type.thread-snapshot-frames true})}
      {::type.thread-snapshot true})
    (finally
      (.resume thread))))

(def vm nil)

(defn attach-vm! []
  (when (nil? vm)
    (.start
     (Thread.
      (fn []
        (let [manager (com.sun.jdi.Bootstrap/virtualMachineManager)]
          (alter-var-root
           #'vm
           (fn [_]
             (some (fn [^com.sun.jdi.connect.AttachingConnector connector]
                     (let [args (.defaultArguments connector)]
                       (when-let [pidarg ^com.sun.jdi.connect.Connector$Argument (.get args "pid")]
                         (.setValue pidarg (String/valueOf (.pid (java.lang.ProcessHandle/current))))
                         (.attach connector args))))
                   (.attachingConnectors manager))))))))))

#_(defn get-stack-variables []
  (let [thread (Thread/currentThread)
        old-name (.getName thread)
        tmp-name (str (random-uuid))]
    (.setName thread tmp-name)
    (try
      #_(let [depth (unchecked-dec-int
                     (.walk (StackWalker/getInstance StackWalker$Option/SHOW_HIDDEN_FRAMES)
                            (reify java.util.function.Function
                              (apply [_ s] (.count ^java.util.stream.Stream s)))))])
      @(future
         (get-variable-values vm tmp-name depth))
      (finally (.setName thread old-name)))))

(defn snapshot-current-thread []
  (let [tn (.getName (Thread/currentThread))
        this-clsname (.getName (class snapshot-current-thread))
        stack-trace (.getStackTrace (Thread/currentThread))
        snap @(future
                (snapshot-thread (vm-thread-named vm tn)))]
    (-> snap
        (update :frames
                (fn [frames]
                  (into (empty frames)
                        (comp
                         (drop-while
                          #(not= this-clsname
                                 (.name
                                  (.declaringType ^com.sun.jdi.Location (:location %)))))
                         (map-indexed (fn [i frame]
                                        (assoc frame :trace-element
                                               (aget stack-trace (inc i))))))
                        frames))))))

(def ^:dynamic *debug-ctx*
  {::debug-error (fn [e] (throw e))
   ::report-ui-error (fn [e])})

(defn handle-caught-error [e]
  (if (App/_onUIThread)
    (do ((::report-ui-error *debug-ctx*) (snapshot-current-thread))
        (throw e))
    ((::debug-error *debug-ctx*) e)))

(comment
  (attach-vm!)

  (defn --f [x]
    (let [y 4]
      (snapshot-current-thread)))
  (:line (meta (var --f)))

  (tap>
   (let [a 0]
     (--f a)))
  (/ 0)

  (def --x nil)


  (Thread/setDefaultUncaughtExceptionHandler
   (reify Thread$UncaughtExceptionHandler
     (uncaughtException [_ thread throwable]
       (chic.debug/println-main "oh no"))))
  (let [x 4]
    (/ 0))

#!
  )

(alter-var-root
 #'puget.color.ansi/sgr-code
 (constantly
  {:none        0
   :bold        1
   :underline   3
   :blink       5
   :reverse     7
   :hidden      8
   :strike      9
   :black      30
   :red        31
   :green      32
   :yellow     33
   :blue       34
   :magenta    35
   :cyan       36
   :white      37
   :fg-256     38
   :fg-reset   39
   :bg-black   40
   :bg-red     41
   :bg-green   42
   :bg-yellow  43
   :bg-blue    44
   :bg-magenta 45
   :bg-cyan    46
   :bg-white   47
   :bg-256     48
   :bg-reset   49
   :dark-grey  90
   :br-red     91
   :br-green   92
   :br-yellow  93
   :br-blue    94
   :br-magenta 95
   :br-cyan    96
   :br-white   97
}))

(alter-var-root
 #'puget/*options*
 (fn [opts]
   (puget/merge-options
    opts
    {:print-color true
     :color-scheme
     {:delimiter [:bold :dark-grey]
      :tag       [:blue]

      :nil       [:magenta]
      :boolean   [:br-magenta]
      :number    [:br-cyan]
      :string    [:bg-green]
      :character [:bg-green]
      :keyword   [:br-magenta]
      :symbol    nil

      :function-symbol [:br-yellow]
      :class-delimiter [:dark-grey]
      :class-name      [:bold :br-red]}})))

(defn puget-prn [x]
  (println-main (puget/pprint-str x)))
