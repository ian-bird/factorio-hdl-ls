(ns to-fc.annealing 
  (:require
   [clojure.walk :as walk]))

(defn distance
  "returns the distance between two nodes"
  [[x1 y1] [x2 y2]]
  (max (abs (- x1 x2)) (abs (- y1 y2))))

(defn center-of-mass
  "given a list of nodes and their positions, calculate the average position.
   This is needed for the annealing process."
  [nodes->positions]
  (if (= 0 (count nodes->positions))
    [0 0]
    (->> nodes->positions
         vals
         (reduce (fn [[sum-x sum-y] [x y]] [(+ sum-x x) (+ sum-y y)]) [0 0])
         (map #(int ( / % (count nodes->positions)))))))

(defn square
  "draw a square with the given radius"
  [r]
  (let [line (range (- r) (inc r))]
    (distinct (concat (map #(vector (- r) %) line)
                      (map #(vector % r) line)
                      (map #(vector r %) line)
                      (map #(vector % (- r)) line)))))

(def spiral "get an infinitely long spiral" (mapcat square (range)))

(defmacro loop-until-repeat
  "add in book keeping for previous inputs
   and exit returning vector of arguments if
   repeated input is encountered"
  {:clj-kondo/lint-as 'clojure.core/loop}
  [loop-args body]
  (let [inputs-set (gensym "inputs")
        loop-arg-names (mapv first (partition 2 loop-args))]
    `(loop [~@loop-args ~inputs-set
            #{}]
       (if (~inputs-set ~loop-arg-names)
         ~loop-arg-names
         ~(walk/prewalk (fn [sym]
                          (if (and (list? sym) (= 'recur (first sym)))
                            (let [args (rest sym)]
                              `(recur ~@args
                                      (conj ~inputs-set ~loop-arg-names)))
                            sym))
                        body)))))

(defn heat
  "expand nodes to make room for placing new ones between them"
  [nodes->positions]
  (update-vals nodes->positions #(map (partial * 2) %)))

(defn cool
  "contract nodes as tightly as possible"
  [nodes->positions]
  (first
   (loop-until-repeat
    [; center on 0,0 to reduce unecessary moves to origin
     nodes->positions
     (update-vals nodes->positions
                  #(map - % (center-of-mass nodes->positions)))
       ; flip orientation every cycle so that nodes dont end up
       ; skewed across one axis
     vertically? true]
    (recur
        ; do a reduction so that moves made by other nodes this cycle
        ; are registered and taken into account, preventing several
        ; nodes moving into the same unoccupied spot.
     (reduce (fn [new-map [node-name [y x]]]
               (let [move-to-center
                     (fn [v]
                       (cond (> v 0) (dec v)
                             (< v 0) (inc v)
                                  ; jitter nodes on the axis to stop
                                  ; them clumping along it
                             (= v 0) (+ ([-1 1] (mod (+ y x) 2)) v)))
                     maybe-move (if vertically?
                                  [(move-to-center y) x]
                                  [y (move-to-center x)])]
                 (if (not-any? #(= maybe-move %) (vals new-map))
                   (assoc new-map node-name maybe-move)
                   new-map)))
             nodes->positions
             nodes->positions)
     (not vertically?)))))

(defn place-node
  "place a node as close as possible to the desired position without
   displacing any existing nodes"
  [node->position existing-nodes->positions]
  (let [desired-position (first (vals node->position))
        closest-empty (->> spiral
                           (map #(map + desired-position %))
                           (filter #(not-any? (partial = %) (vals existing-nodes->positions))) 
                           first
                           vec)
        with-new-pos (update-vals node->position (fn [_] closest-empty))]
    (merge with-new-pos existing-nodes->positions)))