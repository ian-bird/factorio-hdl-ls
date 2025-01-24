(ns scratchpad
  (:require
   [blueprint-serialization]
   [core]))

(core/compile
 (def clock
   (fn [count-to]
     (do (def clock-input (* 0 0))
         (def clock-gated (cond (< clock-input count-to) clock-input))
         (def inc-clock (+ clock-gated 1))
         (def clock-output (cond (> inc-clock (/ count-to 2)) (* 1 1)))
         (assoc clock-input inc-clock)
         clock-output)))
 (clock 30))