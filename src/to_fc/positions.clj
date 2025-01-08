(ns to-fc.positions
  (:require [clojure.math :as math]
            [to-fc.graph :as graph]
            [to-fc.tree :as tree]))

(defn distance
  "returns the distance between two points"
  [position-a position-b]
  (->> (map - position-a position-b)
       (map #(* % %))
       (reduce + 0)
       math/sqrt))

(defn center-of-mass
  "given a list of nodes and their positions, calculate the average position.
   This is needed for the annealing process."
  [nodes->positions]
  (if (= 0 (count nodes->positions))
    [0 0]
    (->> nodes->positions
         vals
         (reduce (fn [[sum-x sum-y] [x y]] [(+ sum-x x) (+ sum-y y)]) [0 0])
         (mapv #(int (/ % (count nodes->positions)))))))

(defn nearest-empty-spot
  "find the nearest empty spot along an axis from the given position"
  [node->positions desired-position]
  ; determine which direction has an empty spot closest to the desired spot
  (let [dirs [[0 1] [0 -1] [1 0] [-1 0]]]
    (->> (range)
         (map (fn [d] (map (fn [[y x]][(* y d) (* x (int (/ d 2)))]) dirs)))
         (map (partial map (partial map + desired-position)))
         (remove (partial every? (partial (set (vals node->positions)))))
         first
         (remove (partial (set (vals node->positions))))
         first
         vec)))

(defn ripple-nodes
  "given a from and a to, ripple every node over towards it.
   from and to MUST share one x or y value minimum."
  [nodes->positions from to]
  (loop [nodes->positions nodes->positions
         from from
         to to]
    (if (not (= from to))
      (let [axis (map #(if (= 0 %) 0 (/ % (abs %))) (map - from to))
            to-ripple (mapv + axis to)]
        (recur (update-vals nodes->positions #(if (= to-ripple %) to %))
               from
               to-ripple))
      nodes->positions)))

(defn determine-positions
  "given a map of entity numbers to adjacent entities,
   return a map of entity numbers to positions."
  [combinator-graph]
  (let [; the order we place entities needs to be by placing the most
        ; widely connected nodes first, then going to the ones they connect
        ; to, placing them in order of how many connections they have, etc.
        ; So a kind-of oddly ordered breadth first search.
        order (->> combinator-graph
                   graph/class->coll-of-graphs
                   (sort-by count >)
                   (mapcat (fn [combinator-graph]
                             (let [root (graph/most-central-node
                                         combinator-graph)]
                               (-> combinator-graph
                                   (graph/minimum-spanning-tree (constantly 1))
                                   (graph/to-tree root)
                                   tree/breadth-first)))))]
    ; for each node in the given order, if it connects to one that's
    ; already been encountered, ripple space to place it at the desired
    ; spot
    (reduce (fn [nodes->positions node]
              (let [has-connections? (some #(nodes->positions %)
                                           (combinator-graph node))
                    com (->> nodes->positions
                             (filter (fn [[k _]] ((combinator-graph node) k)))
                             (into {})
                             center-of-mass)
                    where-to-place (if has-connections?
                                     com
                                     (nearest-empty-spot nodes->positions
                                                         (center-of-mass
                                                          nodes->positions)))
                    merge-with (if has-connections?
                                 (ripple-nodes
                                  nodes->positions
                                  com
                                  (nearest-empty-spot nodes->positions com))
                                 nodes->positions)]
                (merge merge-with {node where-to-place})))
            {}
            order)))
