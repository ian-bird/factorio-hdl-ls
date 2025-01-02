(ns fc-positions
  (:require
   [annealing]
   [graph]))

(defn valid-graph?
  "returns true if all connections between nodes are shorter than
   or equal to the max length for connections"
  [edges nodes->positions max-edge-len]
  (every? (fn [[a b]]
            (<= (annealing/distance (nodes->positions a) (nodes->positions b))
                max-edge-len))
          edges))

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
                                   (graph/remove-cycles root)
                                   (graph/acyclic-graph->tree root)
                                   graph/breadth-first)))))]
    ; for each node in the given order, if it connects to one that's
    ; already been encountered, heat the map, place it between the ones it
    ; connects to, then cool down, otherwise just place on top while cool.
    (reduce (fn [nodes->positions node]
              (if (some #(contains? nodes->positions %) (combinator-graph node))
                (->> nodes->positions
                     annealing/heat
                     (annealing/place-node
                      {node (->> nodes->positions
                                 (filter (fn [[k _]]
                                           (contains? (combinator-graph node)
                                                      k)))
                                 (into {})
                                 annealing/center-of-mass)})
                     annealing/cool)
                (annealing/place-node {node (annealing/center-of-mass
                                             nodes->positions)}
                                      nodes->positions)))
            {}
            order)))
