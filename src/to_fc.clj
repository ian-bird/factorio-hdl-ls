(ns to-fc 
  (:require
   [clojure.set :as set]
   [clojure.walk :as walk]))

(defn merge-overlapping-sets
  "given a coll of sets, merge all ones whose intersection is not the empty set.
   This replaces all sets with a non-empty intersection with a single set whose
   value is the union of the component sets."
  [sets]
  (loop [sets sets
         result []]
    (if (empty? sets)
      result
      (recur (filter #(empty? (set/intersection (first sets) %)) sets)
             (->> sets
                  rest
                  (remove #(empty? (set/intersection (first sets) %)))
                  (apply set/union (first sets))
                  (conj result))))))

(defn get-terminals
  "returns a set of all the combinators that a gensym goes to.
   Each element is of the format {:input combinator#} or 
   {:output combinator#}"
  [tacs gensym]
  (let [entity#-outputs (->> tacs
                             (map-indexed #(vector (last %2) (inc %1)))
                             (filter #(= gensym (% 0)))
                             (map second))
        entity#-inputs (->> tacs
                            (map-indexed #(vector (drop-last %2) (inc %1)))
                            (filter (fn [tac] (some #(= gensym %) (first tac))))
                            (map second))]
    (set (concat (map #(hash-map :output %) entity#-outputs)
                 (map #(hash-map :input %) entity#-inputs)))))

(defn terminal->half-wire
  "converts the terminal structure into half of a wire vector"
  [terminal]
  (if (:input terminal)
    [1 (:input terminal)]
    [3 (:output terminal)]))

(defn extract-gensyms
  "get all the unique gensyms used in a series of tacs"
  [tacs]
  (->> tacs
       (mapcat rest)
       (filter symbol?)
       set))

(defn determine-wires
  "calculates wire routing between combinators specified using tac code."
  [tacs]
  (->> tacs
       extract-gensyms
       (mapcat (fn [gensym]
                 (->> gensym
                      (get-terminals tacs)
                      (map terminal->half-wire)
                      (partition 2 1))))
       (mapv #(vec (apply concat %)))))

(defn group-into-networks
  "given a list of wires, group them into sets of connected terminals"
  [wires]
  (merge-overlapping-sets (map (fn [[a1 a2 b1 b2]] #{[a1 a2] [b1 b2]}) wires)))

(defn gensyms->signals
  "given a coll of tacs replace all gensysms with appropriate
   signals. shadowing is prevented by tracing wire networks."
  [tacs]
  (let [wires (determine-wires tacs)
        terminal-networks (group-into-networks wires)
        gensyms (extract-gensyms tacs)
        gensyms->half-wires (->> gensyms
                                 (map #(vector %
                                               (first (get-terminals tacs %))))
                                 (into {})
                                 (#(update-vals % terminal->half-wire)))
        base-mapping (->> terminal-networks
                          (map #(vector % #{}))
                          (into {})
                          (#(assoc % nil #{})))
        half-wires->network (->> terminal-networks
                                 (mapcat (fn [network]
                                           (map #(vector % network) network)))
                                 (into {}))
        network->gensyms (reduce (fn [mapping [gensym half-wire]]
                                   (update mapping
                                           (half-wires->network half-wire)
                                           #(conj % gensym)))
                                 base-mapping
                                 gensyms->half-wires)
        gensym-groups (vals network->gensyms)
        signal-names (map (fn [i] {:type "virtual" :name (str "signal-" i)})
                          (range 9))]
    (if (some #(< 10 (count %)) gensym-groups)
      (throw (Exception. "circuit network too big! Redesign determine-wires"))
      (walk/prewalk-replace
       (->> gensym-groups
            (mapcat (fn [gensym-group]
                      (map #(vector %1 %2) gensym-group signal-names)))
            (into {}))
       tacs))))

(defn wires->combinator-graph
  "generate a hashmap of combinator numbers to the combinators they connect to"
  [wires]
  (->> wires
       (mapcat (fn [[_ a _ b]] [[a b] [b a]]))
       (reduce (fn [m [k v]] (merge-with #(set/union %1 %2) m {k #{v}})) {})))

(defn tac->fc [& tac-statements]
  )



(wires->combinator-graph (determine-wires '((tac* 0 0 G__17787)
                                            (tac* 0 1 G__17788)
                                            (tac* 0 2 G__17789)
                                            (tac* G__17787 1 G__17790)
                                            (tac= 0 G__17788 G__17790 G__17790)
                                            (tac* G__17787 1 G__17793)
                                            (tac= 0 G__17222 G__17793 G__17793))))