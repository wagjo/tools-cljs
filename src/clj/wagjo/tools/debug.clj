;; Copyright (C) 2013, Jozef Wagner. All rights reserved.

(ns wagjo.tools.debug
  "Debugging tools.")

;;;; Implementation details

;;;; Public API

(defmacro assert
  "Assert macro which prints to console."
  [expr & messages]
  `(.assert js/console ~expr (str ~@(interpose \newline messages))))

(defmacro breakpoint
  "Invokes debugger."
  []
  `(~'js* "debugger"))

;;;; Testing

(comment

  (macroexpand-1 '(assert (= 1 2) "orly" "hai"))
  (macroexpand-1 '(breakpoint))

)
