;; Copyright (C) 2013, Jozef Wagner. All rights reserved.

(ns wagjo.tools.profile
  "Profiling tools.")

;;;; Implementation details

;;;; Public API

(defmacro with-profile
  "Creates chrome dev profile."
  [label & body]
  `(do
     (.profile js/console ~label)
     ~@body
     (.profileEnd js/console)))

(defmacro with-timer
  "Measure time of the body."
  [label & body]
  `(do
     (.time js/console ~label)
     ~@body
     (.timeEnd js/console ~label)))

(defmacro timestamp
  "Places timestamp in timeline view."
  [& messages]
  `(.timeStamp js/console (str ~@(interpose \newline messages))))

(defmacro now
  "Returns a timestamp, measured in milliseconds,
  accurate to one thousandth of a millisecond."
  []
  `(.now window.performance))

(defmacro trace
  "Record time of a given trace, named by label."
  [label]
  `(.trace! wagjo.tools.profile/global-tracer ~label))

(defmacro benchmark
  "Runs benchmark."
  [label repeats loops let-spec & label-fns]
  (let [r (gensym)
        l (gensym)
        gensym-spec `[~r ~repeats
                      ~l ~loops]
        let-spec* (vec (concat gensym-spec let-spec))
        bench-fn (fn [[label body]] [`(dotimes [~'_ ~l] ~body)
                                    `(wagjo.tools.profile/trace ~label)])
        bench-body (mapcat bench-fn (partition 2 label-fns))]
    `(let ~let-spec*
       (wagjo.tools.profile/tracer-reset!)
       (dotimes [~'_ ~r]
         (wagjo.tools.profile/tracer-start! ~r)
         ~@bench-body
         (wagjo.tools.profile/tracer-stop-benchmark! ~label ~l ~r)))))


;;;; Testing

(comment

  (macroexpand-1 '(assert (= 1 2) "orly" "hai"))
  (macroexpand-1 '(breakpoint))
  (macroexpand-1 '(now))
  (macroexpand-1 '(benchmark "obj vs array" 50 10000
                             [ar (array 1 2 3)]
                             "array 1"
                             (aget ar 1)
                             "array 2"
                             (aget ar 2)))







)
