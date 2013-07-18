;; Copyright (C) 2013, Jozef Wagner. All rights reserved.

(ns wagjo.tools.log
  "Loging tools."
  (:refer-clojure :exclude [count]))

;;;; Implementation details

(defn- log*
  "Constructs log call."
  [method messages]
  `(~method js/console (str ~@(interpose \newline messages))))

;;;; Public API

(defmacro clear-console!
  "Clears console."
  [& messages]
  `(.clear js/console))

(defmacro with-group
  "Groups logs in given group."
  [label & body]
  `(do
     (.group js/console ~label)
     ~@body
     (.groupEnd js/console)))

(defmacro with-collapsed-group
  "Groups logs in given group."
  [label & body]
  `(do
     (.groupCollapsed js/console ~label)
     ~@body
     (.groupEnd js/console)))

(defmacro count
  "Prints messages to console, separated by newline.
  Prints number of times that count has been called."
  [& messages]
  (log* '.log messages))

(defmacro log
  "Prints messages to console, separated by newline."
  [& messages]
  (log* '.log messages))

(def ^:macro info #'log)

(def ^:macro debug #'log)

(defmacro warn
  "Prints warn messages to console, separated by newline."
  [& messages]
  (log* '.warn messages))

(defmacro error
  "Prints error messages to console, separated by newline."
  [& messages]
  (log* '.error messages))

(defmacro dir
  "Prints object to console, interactivelly."
  [o]
  `(.dir js/console o))

(defmacro stack-trace
  "Prints stack trace to console."
  []
  `(.trace js/console))

;;;; Testing

(comment

  (macroexpand-1 '(log "foo" "bar" 1))
  (macroexpand-1 '(error "foo" "bar" 1))
  (macroexpand-1 '(info "foo" "bar" 1))
  (macroexpand-1 '(warn "foo" "bar" 1))
  (macroexpand-1 '(debug "foo" "bar" 1))
  (macroexpand-1 '(with-group "my groupa"
                    (+ 4 5)
                    (log "foo")))

)
