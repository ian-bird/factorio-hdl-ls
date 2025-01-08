(ns to-fc.graph 
  (:require
   [clojure.set :as set]))

(defn combine-sets
  "given a coll of sets, merge all ones whose intersection is not the empty set.
   This replaces all sets with a non-empty intersection with a single set whose
   value is the union of the component sets."
  [sets]
  (reduce (fn [coll-of-sets set]
            (let [matching-fn (fn [set-from-coll]
                                (not-empty (set/intersection set-from-coll set)))
                  matches (filter matching-fn coll-of-sets)
                  don't-match (remove matching-fn coll-of-sets)]
              (conj don't-match (apply set/union set matches))))
          []
          sets))

(defn class->coll-of-graphs
  "given a graph that includes potentially non-connected graphs,
   split them up to a list of fully traversable graphs"
  [nodes->adjacent-nodes]
  (->> nodes->adjacent-nodes
       (map (fn [[node adjacents]] (conj adjacents node)))
       combine-sets
       (map #(select-keys nodes->adjacent-nodes %))))

(defn to-tree
  "converts a graph !!WITH NO CYCLES!! into a tree"
  [acyclic-graph root]
  (vec (concat [root]
               (map (partial to-tree acyclic-graph)
                    (acyclic-graph root)))))

(defn max-depth
  "gets the maximum nesting depth of a form"
  [form]
  (if (coll? form) (inc (apply max 0 (map max-depth form))) 0))

(defn minimum-spanning-tree
  "given a graph of connections and a map of node pairs to their weight,
   find a tree the minimizes the max edge weight that occrs."
  ; graph is of form {node #{connectec-nodes}, ...}
  ; edge weights of form {[node-a node-b] distance}
  [graph edge-weights]
  (loop [visited #{(ffirst graph)}
         result {}]
    (let [unvisited (remove visited (keys graph))]
      (if (empty? unvisited)
        result
        (let [smallest-edge (apply min-key
                                   #(edge-weights %)
                                   (for [visited-node visited
                                         unvisited-node unvisited]
                                     [visited-node unvisited-node]))
              visited-node (first (filter visited smallest-edge))
              unvisited-node (first (remove visited smallest-edge))]
          (recur (conj visited unvisited-node)
                 (if (result visited-node)
                   (update result visited-node #(conj % unvisited-node))
                   (assoc result visited-node #{unvisited-node}))))))))

(defn most-central-node
  "gets the node thats can reach the furthest node in the fewest possible steps"
  [graph]
  (apply min-key
    (fn [node]
      (->> node
           (to-tree (minimum-spanning-tree graph (constantly 1)))
           max-depth))
    (distinct (concat (keys graph) (mapcat #(into [] %) (vals graph)))))) 
