(ns to-fc.positions 
  (:require
    [clojure.set :as set]))

(defn add-edge
  "adds an edge between two nodes in the positioned graph"
  [positioned-graph from to]
  (-> positioned-graph
      (update from (fn [v] (update v :adjacents #(set/union % #{to}))))
      (update to (fn [v] (update v :adjacents #(set/union % #{from}))))))

(defn add-node
  "adds a node into the positioned graph with the given position
   and adjacents, and optionally the entity number."
  ([positioned-graph id position adjacents]
   (reduce (fn [graph adjacent] (add-edge graph id adjacent))
           (assoc positioned-graph id {:position position :adjacents #{}})
           adjacents))
  ([positioned-graph position adjacents]
   (add-node positioned-graph
             {:pole (inc (apply max 0 (mapcat vals (keys positioned-graph))))}
             position
             adjacents)))

(defn missing-vals
  "find the values we need to insert between from and to
   such that no jump is greater than max, while also having
   nice alignment."
  [from to max offset]
  (->> (range)
       (map #(* max %))
       (map #(-  % offset))
       (drop-while #(>= from %))
       (take-while #(> to %))))

(defn terminal-networks->positioned-graph
  "given a list of terminal networks, build a naive positioned graph"
  [terminal-networks]
  (reduce (fn [graph terminal-network]
            (reduce (fn [graph terminal]
                      (assoc graph
                             terminal {:position [0
                                                  (+ (if (:output terminal) 0 1)
                                                     (* 2 (first (vals terminal))))]
                                       :adjacents (set/difference terminal-network
                                                                  #{terminal})}))
                    graph
                    terminal-network))
          {}
          terminal-networks))

(defn interpose-fn
  "given an array, interpose an applied function that takes the elements
   that appear immediately before and after each element"
  [f coll]
  (cons (first coll) (mapcat (fn [[a b]] [(f a b) b]) (partition 2 1 coll))))

(defn create-wire
  "creates power poles if positions do not exist, otherwise uses existing ones
   and builds an edge between nodes"
  [graph from-pos to-pos]
  (let [maybe-add (fn [graph position]
                    (if (some #(= position %) (map :position (vals graph)))
                      graph
                      (add-node graph position #{})))
        graph-with-poles (reduce maybe-add graph [from-pos to-pos])
        lookup-name (fn [graph position]
                      ((set/map-invert (update-vals graph :position))
                       position))]
    (add-edge graph-with-poles
              (lookup-name graph-with-poles from-pos)
              (lookup-name graph-with-poles to-pos))))

(defn add-power-poles
  "given a list of terminal networks, generate a positioned graph
   including all the power poles needed to link everything."
  [terminal-networks max-len]
  (let [initial-graph (terminal-networks->positioned-graph terminal-networks)]
    (->> (map vector (map inc (range)) terminal-networks)
         (reduce
          (fn [graph [index terminal-network]]
            (let [terminal-positions (->> terminal-network
                                          (select-keys graph)
                                          ((fn [m] (update-vals m :position))))
                  vertical-wiring (->> terminal-positions
                                       vals
                                       (map #(% 1))
                                       sort
                                       (interpose-fn
                                        #(missing-vals %1 %2 max-len index))
                                       flatten
                                       (map #(vector index %))
                                       (partition 2 1))
                  horizontal-wiring
                  (->> terminal-positions
                       vals
                       (map #(% 1))
                          ;; for each row, generate all the values between
                          ;; 0 and the index, using the offset based on the
                          ;; row-num + max-len/2. This means that diagonal
                          ;; stripes are staggered, and the ones for wires
                          ;; traveling horizontally will not intersect with
                          ;; ones for wires traveling vertically.
                       (mapcat (fn [row-num]
                                 (->> [0 index]
                                      (interpose-fn #(missing-vals
                                                      %1
                                                      %2
                                                      max-len
                                                      (+ row-num
                                                         (quot max-len 2))))
                                      flatten
                                      (map #(vector % row-num))
                                      (partition 2 1)))))
                  wiring (concat horizontal-wiring vertical-wiring)]
              (reduce (fn [graph [from to]] (create-wire graph from to))
                      graph
                      wiring)))
          initial-graph)
         ((fn [m]
            (update-vals m
                         (fn [v]
                           (if (= 0 ((v :position) 0))
                             (update v :adjacents #(set (filter :pole %)))
                             v))))))))
