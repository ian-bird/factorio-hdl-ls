(ns spec.shorthand
  (:require
   [clojure.spec.alpha] 
   [extra-walk-fns :as ewf]
   [keyword-modifiers :as km]))

 (defmacro do-specdef
  [name structure]
  (let [vec-kw (km/coerce-qualified-kw (gensym (str (km/unqualify name) "-vec")))]
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
                   (clojure.spec.alpha/def ~(km/coerce-qualified-kw name)
                     (clojure.spec.alpha/coll-of ~vec-kw
                                                 :kind vector?
                                                 :min-count 1)))
            * `(do (spec.shorthand/do-specdef ~vec-kw ~(structure 0))
                   (clojure.spec.alpha/def ~(km/coerce-qualified-kw name)
                     (clojure.spec.alpha/coll-of ~vec-kw :kind vector?)))))
      ; if we're doing a map then we need to create a keys spec and then
      ; also create specs for all the values in the map.
      (map? structure)
        (let [kws (into {}
                        (map (fn [[k _]] [k (km/coerce-qualified-kw k)])
                          structure))
              x (gensym "x")]
          `(do ~@(map (fn [[k v]] `(spec.shorthand/do-specdef ~(kws k) ~v))
                   structure)
               (clojure.spec.alpha/def ~(km/coerce-qualified-kw name)
                 (fn [~x]
                   (and (clojure.spec.alpha/valid?
                          (clojure.spec.alpha/keys :req-un [~@(vals kws)])
                          ~x)
                        (= (set (map #(km/coerce-qualified-kw %) (keys ~x)))
                           (set [~@(vals kws)])))))))
      ; a list means we're doing a spec combination function like or
      ; or that we're inserting forms from elsewhere.
      ; so create the manual validation code and then create the specs
      ; for all the patterns in the body of the list.
      (list? structure)
        (case (first structure)
          or (let [kws (into {}
                             (map #(vector %
                                           (km/coerce-qualified-kw (gensym "or")))
                                  (rest structure)))
                   x (gensym "x")]
               `(do ~@(map (fn [e] `(spec.shorthand/do-specdef ~(kws e) ~e))
                           (rest structure))
                    (clojure.spec.alpha/def ~(km/coerce-qualified-kw name)
                      (fn [~x]
                        (or ~@(map (fn [e]
                                     `(clojure.spec.alpha/valid? ~(kws e) ~x))
                                   (rest structure)))))))
          (if ((requiring-resolve 'clojure.test/function?) (first structure))
            `(spec.shorthand/do-specdef ~name ~(eval structure))
            (throw (Exception. "List in specdef must be statically resolvable
                                fn call or or block."))))
      ; a function or symbol that resolves to a function can be directly
      ; inserted; so do that.
      ((requiring-resolve 'clojure.test/function?) structure)
        `(clojure.spec.alpha/def ~(km/coerce-qualified-kw name) ~structure))))



(defmacro specdef
  "destructures the input structure and creates all the prerequisite 
   spec defs needed to properly define the original spec."
  [name structure]
  (let [specs ((fn flatten-dos [x]
                 (if (= (first x) 'do) (mapcat flatten-dos (rest x)) [x]))
               (ewf/expand-these-macros ['do-specdef]
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
