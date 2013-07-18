;; Copyright (C) 2013, Jozef Wagner. All rights reserved.

(ns wagjo.tools.profile
  "Profiling tools."
  (:require-macros [wagjo.tools.profile :as prof]
                   [wagjo.tools.log :as log]))

;;;; Implementation details

(deftype Tracer [^:mutable base-time
                 ^:mutable index ^:mutable checkpoint
                 ^:mutable labels ^:mutable times]
  Object
  (tracer-reset! [t]
    (set! (.-base-time t) nil)
    (set! (.-index t) nil)
    (set! (.-checkpoint t) nil)
    (set! (.-labels t) (array))
    (set! (.-times t) (array)))
  (tracer-start! [t checkpoint]
    (set! (.-base-time t) (prof/now))
    (set! (.-index t) 0)
    (set! (.-checkpoint t) checkpoint))
  (tracer-stop! [t]
    (.trace! t nil)
    (let [times (.-times t)]
      (when (zero? (mod (count (aget times 0)) (.-checkpoint t)))
        (let [get-avg #(let [ms (sort (seq (aget times %)))]
                         (nth ms (/ (count ms) 2) 0))
              t-to-str #(let [label (aget (.-labels t) %)
                              pt (if (> % 0) (get-avg (dec %)) 0)]
                          (str label (when label " ")
                               "[" (.toFixed (- (get-avg %) pt) 3)
                               "]"))
              t-seq (map t-to-str (range (.-index t)))]
          (log/debug (str (apply str (interpose \newline t-seq)) " ("
                          (.toFixed (get-avg (dec (.-index t))) 3)
                          ") on " (count (aget times 0))))))))
  (tracer-stop-benchmark! [t label iterations samples]
    (let [times (.-times t)]
      (when (zero? (mod (count (aget times 0)) (.-checkpoint t)))
        (log/with-group (str "Results for benchmark \"" label
                             "\" for " iterations
                             " iterations, median from "
                             samples " runs")
          (let [get-avg #(let [ms (sort (seq (aget times %)))]
                           (nth ms (/ (count ms) 2) 0))
                t-to-str
                #(let [label (aget (.-labels t) %)
                       pt (if (> % 0) (get-avg (dec %)) 0)
                       ns (str "000" (.toFixed (- (get-avg %) pt) 3))
                       zfs (str (.substr ns (- (.-length ns) 7))
                                "ms - "
                                label)]
                   zfs)
                t-seq (map t-to-str (range (.-index t)))]
            (doseq [t t-seq] (log/debug t)))))))
  (trace! [t label]
    (let [index (.-index t)
          times (.-times t)
          current-times (or (aget times index) (array))]
      (.push current-times (- (prof/now) (.-base-time t)))
      (aset (.-labels t) index label)
      (aset times index current-times)
      (set! (.-index t) (inc index)))))

(def global-tracer (Tracer. nil nil nil (array) (array)))

;;;; Public API

(defn tracer-reset!
  "Resets tracer. Deletes stored measurements."
  []
  (.tracer-reset! global-tracer))

(defn tracer-start!
  "Starts new measurement. Checkpoint determines number of
   measurements, after which report is printed into console.
   Without checkpoint, report is printed after each measurement."
  ([]
     (tracer-start! 1))
  ([checkpoint]
     (.tracer-start! global-tracer checkpoint)))

(defn tracer-stop!
  "Finishes measurement. Prints report each time
   checkpoint is reached."
  []
  (.tracer-stop! global-tracer))

(defn tracer-stop-benchmark!
  "Finishes measurement. Prints report each time
   checkpoint is reached."
  [label iterations samples]
  (.tracer-stop-benchmark! global-tracer label iterations samples))

;;;; Testing

(comment

)
