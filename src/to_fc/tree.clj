(ns to-fc.tree)

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