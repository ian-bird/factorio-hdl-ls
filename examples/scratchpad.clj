(ns scratchpad
  (:require
   [core :as hdl]))

(hdl/compile (def mem ; memory cell, stores value on set != 0
               (fn [set input]
                 (do (def mem-input (cond (!= set 0) (* 1 input)))
                     (def mem-output (cond (= set 0) mem-input))
                     (assoc mem-output mem-input))))
             (def bus (* 0 0))
             (def set-temp (* 0 1))

             ; create the memory cell
             (mem set-temp bus))