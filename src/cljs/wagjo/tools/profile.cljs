;; Copyright (C) 2013, Jozef Wagner. All rights reserved.

(ns wagjo.tools.profile
  "Profiling tools."
  (:require-macros [wagjo.tools.profile :as prof]
                   [wagjo.tools.log :as log])
  (:require [clojure.string :as cs]))

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
  (tracer-report! [t label iterations]
    (let [title (str "Results for benchmark " label " for "
                     iterations " iterations, median from "
                     (.-checkpoint t) " runs")
          get-avg #(let [ms (sort (seq (aget (.-times t) %)))]
                     (nth ms (/ (count ms) 2) 0))
          t-to-str
          #(let [label (aget (.-labels t) %)
                 pt (if (> % 0) (get-avg (dec %)) 0)
                 r (- (get-avg %) pt)
                 ns (if (>= r 1)
                     (str "\u00A0\u00A0\u00A0\u00A0" (.toFixed r 3) "ms")
                     (str "\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0" (.floor js/Math (* r 1000)) "μs"))
                 ns  (if (>= r 1)
                       (.substr ns (- (.-length ns) 10))
                       (.substr ns (- (.-length ns) 10)))]
             (str ns " " label))
          t-seq (map t-to-str (range (.-index t)))]
      (cons title t-seq)))
  (tracer-log! [t label iterations]
    (let [report (.tracer-report! t label iterations)]
      (log/with-group (first report)
        (doseq [t (rest report)]
          (log/debug t)))
      report))
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

(defn tracer-report!
  "Returns tracer report as seq of strings."
  [label iterations]
  (.tracer-report! global-tracer label iterations))

(defn tracer-log!
  "Returns tracer report as seq of strings. Logs report to console."
  [label iterations]
  (.tracer-log! global-tracer label iterations))

;;;; Testing

(comment

)
