(ns scratchpad
  (:require
   [core]))

(core/compile 
 (core/defn*
   mem-cell
    ; address is an int, others are signals.
   [read-from mem-set mem-read address address-bus]
   (core/let* [mem-input (cond (and (!= mem-set 0) (= address-bus address)) read-from)
               mem-internal (cond (or (= mem-set 0) (!= address-bus address)) mem-input)
               mem-out (cond (and (!= mem-read 0) (= address-bus address)) mem-internal)]
              (assoc mem-input mem-internal)
              mem-out))
 
 (core/defn* mem-bank-2
   [read-bus set read address address-bus]
   (core/let* [top-half (bit-shift-right address-bus 1)
               bottom-half (bit-and address-bus 1)
               mem-set (cond (= top-half address) set)
               mem-read (cond (= top-half address) read)
               output (* 0 0)]
              (assoc (mem-cell read-bus mem-set mem-read 0 bottom-half) output)
              (assoc (mem-cell read-bus mem-set mem-read 1 bottom-half) output) 
              output)) 
 
 (def main-bus (* 0 10))
 (def mem-bus (* 0 20))
 (def setm (* 0 30))
 (def readm (* 0 40))
 (mem-bank-2 main-bus setm readm 0 mem-bus))

