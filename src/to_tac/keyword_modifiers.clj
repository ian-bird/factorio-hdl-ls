(ns to-tac.keyword-modifiers)

(defmacro coerce-qualified-kw
  [x]
  `(if (qualified-keyword? ~x) ~x (keyword (str *ns*) (str (symbol ~x)))))

(defn unqualify
  [x]
  (last ((requiring-resolve 'clojure.string/split) (str x) #"/")))