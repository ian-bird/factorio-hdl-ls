(ns spec.shorthand
  (:require
   [clojure.spec.alpha]
   [clojure.walk :as walk]))

(defmacro coerce-qualified-kw
       [x] 
       `(if (qualified-keyword? ~x) ~x (keyword (str *ns*) (str (symbol ~x)))))

(defn- unqualify
  [x]
  (last ((requiring-resolve 'clojure.string/split) (str x) #"/")))

 (defmacro do-specdef 
  [name structure]
  (let [vec-kw (coerce-qualified-kw (gensym (str (unqualify name) "-vec")))]
    (cond
      ; a vector means we're matching against a vector.
      ; vectors have one argument and + / * allows the user
      ; to define whether empty vectors are valid or not.
      ; also spawns spec defs for the pattern that occurs in
      ; the vector.
      (vector? structure)
        (if (> 2 (count structure))
          (throw (Exception.
                   "specdef error: must specify + or * for vector matching."))
          (case (structure 1)
            + `(do (spec.shorthand/do-specdef ~vec-kw ~(structure 0))
                   (clojure.spec.alpha/def ~(coerce-qualified-kw name)
                     (clojure.spec.alpha/coll-of ~vec-kw
                                                 :kind vector?
                                                 :min-count 1)))
            * `(do (spec.shorthand/do-specdef ~vec-kw ~(structure 0))
                   (clojure.spec.alpha/def ~(coerce-qualified-kw name)
                     (clojure.spec.alpha/coll-of ~vec-kw :kind vector?)))))
      ; if we're doing a map then we need to create a keys spec and then
      ; also create specs for all the values in the map.
      (map? structure)
        (let [kws (into {}
                        (map (fn [[k _]] [k (coerce-qualified-kw k)])
                          structure))]
          `(do ~@(map (fn [[k v]] `(spec.shorthand/do-specdef ~(kws k) ~v))
                   structure)
               (clojure.spec.alpha/def ~(coerce-qualified-kw name)
                 (clojure.spec.alpha/keys :req-un [~@(vals kws)]))))
      ; a list means we're doing a spec combination function like or,
      ; so create the manual validation code and then create the specs
      ; for all the patterns in the body of the list.
      (list? structure)
        (case (first structure)
          or (let [kws (into {}
                             (map #(vector %
                                           (coerce-qualified-kw (gensym "or")))
                               (rest structure)))
                   x (gensym "x")]
               `(do ~@(map (fn [e] `(spec.shorthand/do-specdef ~(kws e) ~e))
                        (rest structure))
                    (clojure.spec.alpha/def ~(coerce-qualified-kw name)
                      (fn [~x]
                        (or ~@(map (fn [e]
                                     `(clojure.spec.alpha/valid? ~(kws e) ~x))
                                (rest structure))))))))
      ; a function or symbol that resolves to a function can be directly
      ; inserted; so do that.
      ((requiring-resolve 'clojure.test/function?) structure)
        `(clojure.spec.alpha/def ~(coerce-qualified-kw name) ~structure))))

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


(defmacro specdef
  "destructures the input structure and creates all the prerequisite 
   spec defs needed to properly define the original spec."
  [name structure]
  (let [specs ((fn flatten-dos [x]
                 (if (= (first x) 'do)
                   (concat (flatten-dos (second x)) (drop 2 x))
                   [x]))
               (expand-specific-macro 'do-specdef
                                      `(do-specdef ~name ~structure)))
        by-name (reduce (fn [h s]
                          (update h
                                  (second s)
                                  (fn [v] (if (nil? v) (list s) (cons s v)))))
                        {}
                        specs)
        x (gensym "x")
        merged (into {}
                     (map (fn [[k v]]
                            [k
                             (if (= (count v) 1)
                               (first v)
                               `(clojure.spec.alpha/def ~k
                                  (fn [~x]
                                    (or ~@(map (fn [e]
                                                 `(clojure.spec.alpha/valid?
                                                   ~(nth e 2)
                                                   ~x))
                                               (reverse v))))))])
                          by-name))]
    `(do ~@(vals merged))))
