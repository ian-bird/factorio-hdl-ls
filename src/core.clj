(ns core)

(defmacro compile
  "compile factorio hdl lisp into a blueprint string"
  {:clj-kondo/ignore true}
  [& fc-lisp-statements]
  `(do (require 'to-tac.converter)
       (->> (to-tac.converter/fc-lisp->tac ~@fc-lisp-statements)
            (apply (requiring-resolve 'to-fc.converter/tac->fc))
            ( (requiring-resolve 'blueprint-serialization/from-sexpr)))))

(defmacro defn*
  "functions the same as clojure's define"
  [name args body]
  `(def ~name (fn [~@args] ~body)))

(defmacro let*
  "functions the same as clojure's let"
  [bindings & body]
  (cond (not (vector? bindings)) (throw (Exception.
                                         "Let bindings must be in vector."))
        (not (even? (count bindings))) (throw (Exception.
                                               "Uneven number of bindings."))
        :else `(do ~@(map (fn [[name val]] `(def ~name ~val))
                          (partition 2 bindings))
                   ~@body)))
