(ns core)

(defmacro compile
  "compile factorio hdl lisp into a blueprint string"
  {:clj-kondo/ignore true}
  [& fc-lisp-statements]
  `(do (require 'to-tac.converter)
       (->> (to-tac.converter/fc-lisp->tac ~@fc-lisp-statements)
            (apply (requiring-resolve 'to-fc.converter/tac->fc))
            ( (requiring-resolve 'blueprint-serialization/from-sexpr)))))