(ns scratchpad
  (:require
   [core]))

;; (core/defn* clock
;;   [count-to]
;;   (core/let* [clock-input (* 0 0)
;;               clock-gated (cond (< clock-input count-to) clock-input)
;;               inc-clock (+ clock-gated 1)
;;               clock-output (cond (> inc-clock (/ count-to 2)) (* 1 1))]
;;              (assoc clock-input inc-clock)
;;              clock-output))

;; (core/defn* alu
;;               ; does basic arithmetic and logic operations. Op selectable
;;   [op a b]
;;   (cond (= op 0) (+ a 1)
;;         (= op 1) (+ a b)
;;         (= op 2) (- a b)
;;         (= op 3) (* a b)
;;         (= op 4) (/ a b)
;;         (= op 5) (bit-or a b)
;;         (= op 6) (bit-and a b)
;;         (= op 7) (bit-xor a b)
;;         (= op 8) (cond (< a b) (+ 0 -1)
;;                        (= a b) (+ 0 0)
;;                        (> a b) (+ 0 1))))

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
 
 (core/defn* mem-bank-8
   [read-bus set read address address-bus]
   (core/let* [top-half (bit-shift-right address-bus 3)
               bottom-half (bit-and address-bus 7)
               mem-set (cond (= top-half address) set)
               mem-read (cond (= top-half address) read)
               output (* 0 0)]
              (assoc (mem-cell read-bus mem-set mem-read 0 bottom-half) output)
              (assoc (mem-cell read-bus mem-set mem-read 1 bottom-half) output)
              (assoc (mem-cell read-bus mem-set mem-read 2 bottom-half) output)
              (assoc (mem-cell read-bus mem-set mem-read 3 bottom-half) output)
              (assoc (mem-cell read-bus mem-set mem-read 4 bottom-half) output)
              (assoc (mem-cell read-bus mem-set mem-read 5 bottom-half) output)
              (assoc (mem-cell read-bus mem-set mem-read 6 bottom-half) output)
              (assoc (mem-cell read-bus mem-set mem-read 7 bottom-half) output)
              output))
 
 (core/defn* mem-bank-64
   [read-bus set read address address-bus]
   (core/let* [top-half (bit-shift-right address-bus 6)
               bottom-half (bit-and (bit-shift-right address-bus 3) 7)
               mem-set-2 (cond (= top-half address) set)
               mem-read (cond (= top-half address) read)
               output (* 0 0)]
              (assoc (mem-bank-8 read-bus mem-set-2 mem-read 0 bottom-half) output)
              (assoc (mem-bank-8 read-bus mem-set-2 mem-read 1 bottom-half) output)
              (assoc (mem-bank-8 read-bus mem-set-2 mem-read 2 bottom-half) output)
              (assoc (mem-bank-8 read-bus mem-set-2 mem-read 3 bottom-half) output)
              (assoc (mem-bank-8 read-bus mem-set-2 mem-read 4 bottom-half) output)
              (assoc (mem-bank-8 read-bus mem-set-2 mem-read 5 bottom-half) output)
              (assoc (mem-bank-8 read-bus mem-set-2 mem-read 6 bottom-half) output)
              (assoc (mem-bank-8 read-bus mem-set-2 mem-read 7 bottom-half) output)
              output)
   )
 
 (def main-bus (* 0 10))
 (def mem-bus (* 0 20))
 (def setm (* 0 30))
 (def readm (* 0 40))
 (mem-bank-8 main-bus setm readm 0 mem-bus))

