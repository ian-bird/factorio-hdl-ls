(ns extra-walk-fns 
  (:require
   [clojure.walk :as walk]))

(defn- expand-specific-macro
  "given a specific macro, expands *only* this function.
   Nothing else is expanded."
  [f form]
  (let [expanded-once (walk/prewalk
                       #(if (and (coll? %)
                                 (or (= (first %) f)
                                     (= (first %)
                                        (symbol (str "spec.shorthand/" f)))))
                          (macroexpand-1 %)
                          %)
                       form)]
    (if (= expanded-once form) form (expand-specific-macro f expanded-once))))

(defn expand-these-macros
  [fs form]
  (reduce (fn [form f] (expand-specific-macro f form)) form fs))