(ns factorio-hdl-ls 
  (:require
   [to-fc]
   [to-tac]
   [blueprint-serialization :as bs]))

(defmacro compile
  [& fc-lisp-statements]
  `(->> (to-tac/fc-lisp->tac ~@fc-lisp-statements)
        (apply to-fc/tac->fc)
        (bs/from-sexpr)))