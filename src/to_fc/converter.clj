(ns to-fc.converter 
  (:require
   [clojure.set :as set]
   [clojure.walk :as walk]
   [to-fc.positions :as pos]))

(defn merge-overlapping-sets
  "given a coll of sets, merge all ones whose intersection is not the empty set.
   This replaces all sets with a non-empty intersection with a single set whose
   value is the union of the component sets."
  [sets]
  (reduce (fn [coll-of-sets set]
            (let [matching-fn (fn [set-from-coll]
                                (seq (set/intersection set-from-coll set)))
                  matches (filter matching-fn coll-of-sets)
                  don't-match (remove matching-fn coll-of-sets)]
              (conj don't-match (apply set/union set matches))))
          []
          sets))

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
    [(:input terminal) 1]
    [(:output terminal) 3]))

(defn extract-gensyms
  "get all the unique gensyms used in a series of tacs"
  [tacs]
  (->> tacs
       (mapcat rest)
       (filter symbol?)
       set))

(defn tacs->wires
  "calculates wire routing between combinators specified using tac code."
  [tacs]
  (->> tacs
       extract-gensyms
       (mapcat (fn [gensym]
                 (->> gensym
                      (get-terminals tacs)
                      (map terminal->half-wire)
                      (partition 2 1))))
       (map #(vec (apply concat %)))
       distinct
       vec))

(defn group-into-networks
  "given a list of wires, group them into sets of connected terminals"
  [wires]
  (merge-overlapping-sets (map (fn [[a1 a2 b1 b2]] #{[a1 a2] [b1 b2]}) wires)))

(defn gensyms->signals
  "given a coll of tacs replace all gensysms with appropriate
   signals. shadowing is prevented by tracing wire networks."
  [tacs]
  ; the overall goal here is to group gensyms into groups based off what
  ; networks they appear on. so we need to grab the wires that the gensym
  ; passes over, and then get the network that wire is a part of. then we
  ; can group gensysms by network, and then associate each one with a
  ; signal that's unique for that network.
  ;
  ; then all we need to do is just replace every occurence of the gensym in
  ; the tacs with its appropriate signal replacement.
  (let [wires (distinct (tacs->wires tacs))
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
        signal-names (concat
                      (map (fn [i] {:type "virtual" :name (str "signal-" i)})
                           (range 9))
                      (map (fn [c] {:type "virtual" :name (str "signal-" c)})
                           (map char (range 65 91))))
        ; we need to do a special case for decider combinators. The output
        ; symbol needs to be replaced with the input symbol, and its
        ; guaranteed to have no impact on the other circuits or networks.
        update-decider-outputs (reduce (fn [update-map tac]
                                         (if (= 5 (count tac))
                                           (assoc update-map
                                                  (nth tac 4) (nth tac 3))
                                           update-map))
                                       (into {} (map #(vector % %) gensyms))
                                       tacs)]
    (if (some #(< 36 (count %)) gensym-groups)
      (throw (Exception. "circuit network too big! Redesign determine-wires"))
      (->> tacs
           (walk/prewalk-replace update-decider-outputs)
           (walk/prewalk-replace
            (->> gensym-groups
                 (mapcat (fn [gensym-group]
                           (map #(vector %1 %2) gensym-group signal-names)))
                 (into {})))
           ; the piped-in symbol isn't part of the entity description, and
           ; its redundant, so drop it.
           (map #(if (= 5 (count %)) (drop-last %) %))))))

(defn wires->combinator-graph
  "generate a hashmap of combinator numbers to the combinators they connect to"
  [num-tacs wires]
  (let [wired-combinators
        (->> wires
             (mapcat (fn [[a _ b _]] [[a b] [b a]]))
             (reduce (fn [m [k v]] (merge-with #(set/union %1 %2) m {k #{v}}))
                     {}))
        missing (remove #(wired-combinators %)
                        (range 1 (inc num-tacs)))]
    (apply conj wired-combinators (map #(vector % #{}) missing))))

(defn one-tac->entity
  "convert a single tac into an entity. 
   !!GENSYM REPLACEMENT MUST HAVE HAPPENED FIRST!!"
  [tac entity-num position]
  ; this converts a tac with some metadata into the format that factorio
  ; blueprints expect for an entity
  ;
  ; almost all the code in here is just to restructure the data into
  ; the format that factorio expects.
  (let [tac->operation {'tac+ "+"
                        'tac- "-"
                        'tac* "*"
                        'tac-div "/"
                        'tac-mod "%"
                        'tac-bit-shift-left "<<"
                        'tac-bit-shift-right ">>"
                        'tac-bit-and "AND"
                        'tac-bit-or "OR"
                        'tac-bit-xor "XOR"}
        tac->comparator
        {'tac> ">" 'tac< "<" 'tac= "=" 'tac!= "!=" 'tac>= ">=" 'tac<= "<="}
        tac-vec (vec tac)]
    (if (tac->operation (first tac))
      {:entity_number entity-num
       :name "arithmetic-combinator"
       :position {:x (position 0) :y (+ (* (position 1) 2) 0.5)}
       :direction 0
       :control_behavior
       {:arithmetic_conditions
        {(if (number? (tac-vec 1)) :first_constant :first_signal) (tac-vec
                                                                   1)
         (if (number? (tac-vec 2)) :second_constant :second_signal) (tac-vec
                                                                     2)
         :operation (tac->operation (tac-vec 0))
         :output_signal (tac-vec 3)}}}
      {:entity_number entity-num
       :name "decider-combinator"
       :position {:x (position 0) :y (+ (* (position 1) 2) 0.5)}
       :direction 0
       :control_behavior
       {:decider_conditions
        {:conditions
         [{:first_signal
           (if (number? (tac-vec 1))
             (throw
              (Exception.
               "ERROR: first argument for predicate cannot be a number."))
             (tac-vec 1))
           (if (number? (tac-vec 2)) :constant :second_signal) (tac-vec 2)
           :comparator (tac->comparator (tac-vec 0))}]
         :outputs [{:signal (if (number? (tac-vec 3))
                              (throw
                               (Exception.
                                "ERROR: consequent cannot be a number."))
                              (tac-vec 3))
                    :copy_count_from_input true}]}}})))

(defn tac->fc
  "convert tac statements into fully formed blueprint struct"
  [& tac-statements]
  (let [wires (tacs->wires tac-statements)
        positions-map (->> wires
                           (wires->combinator-graph (count tac-statements))
                           pos/determine-positions)
        entities (mapv #(one-tac->entity %1 %2 (positions-map %2))
                       (gensyms->signals tac-statements)
                       (range 1 (inc (count tac-statements))))]
    ; return the data in the format that factorio blueprint expects. Essentially
    ; just restructuring into a json-esque structure.
    ; the keywords will automatically be converted into strings.
    {:blueprint {:icons [{:signal {:type "virtual" :name "signal-L"} :index 1}]
                 :entities entities
                 :wires wires
                 :item "blueprint"
                 :version 1}}))
