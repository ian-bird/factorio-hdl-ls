(ns graph 
  (:require
   [clojure.set :as set]))

(defn breadth-first
  "given a form of the structure [a [b [d e]] [c [f g]]] 
   return [a b c d e f g]"
  [form]
  (loop [this-level form result []]
    (let [next-level (vec (apply concat (filter coll? this-level)))
          atoms-on-this-level (remove coll? this-level)]
      (if (empty? this-level)
        result
        (recur next-level (apply conj result atoms-on-this-level))))))

(defn edges
  "returns a list of all the edges between nodes in the graph "
  [nodes->adjacent-nodes]
  (->> nodes->adjacent-nodes
       (mapcat (fn [[node adjacents]] (map #(vector node %) adjacents)))
       (map sort)
       distinct))

(defn class->coll-of-graphs
  "given a graph that includes potentially non-connected graphs,
   split them up to a list of fully traversable graphs"
  [nodes->adjacent-nodes]
  (reduce (fn [coll-of-graphs node]
            (let [matching-fn (fn [graph]
                                (contains? (apply set/union (vals graph)) node))
                  matches (filter matching-fn coll-of-graphs)
                  don't-match (remove matching-fn coll-of-graphs)]
              (conj don't-match
                    (apply merge {node (nodes->adjacent-nodes node)} matches))))
          []
          (keys nodes->adjacent-nodes)))

(defn remove-cycles
  "doesn't allow elements to point to already visited nodes
   but doesn't structure as tree."
  [graph root]
  (loop [result {}
         this-level [root]]
    (let [next-level (->> this-level
                          (mapcat #(graph %))
                          distinct
                          (remove #(or ( contains? result %)
                                       (contains? (set this-level) %))))
          without-cycles (map (fn [node] [node
                                          (set/difference (graph node)
                                                          (set (keys result))
                                                          (set this-level))])
                              this-level)]
      (if (empty? next-level)
        result
        (recur (apply conj result without-cycles) next-level)))))

(defn acyclic-graph->tree
  "converts a graph !!WITH NO CYCLES!! into a tree"
  [acyclic-graph root]
  (vec (concat [root]
               (map (partial acyclic-graph->tree acyclic-graph)
                    (acyclic-graph root)))))

(defn max-depth
  "gets the maximum nesting depth of a form"
  [form]
  (if (coll? form)
    (inc (apply max 0 (map max-depth form )))
    0))

(defn most-central-node
  "gets the node thats can reach the furthest node in the fewest possible steps"
  [graph]
  (apply min-key
         (fn [node]
           (max-depth (acyclic-graph->tree (remove-cycles graph node) node)))
         (distinct (concat (keys graph) (mapcat #(into [] %) (vals graph))))))

