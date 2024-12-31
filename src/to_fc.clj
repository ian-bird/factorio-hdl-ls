(ns to-fc)


(defn decider-control-behavior
  "first-signal can only be a signal. 
   second-val can be a signal or a constant."
  [first-signal second-val output-signal output-one? comparator]
  {:decider_conditions {:conditions [{:first_signal first-signal
                                      (cond (map? second-val) :second_signal
                                            (int? second-val) :constant)
                                      second-val
                                      :comparator comparator}]
                        :outputs [{:signal output-signal
                                   :copy_count_from_input (complement
                                                           output-one?)}]}})

(defn arithmetic-control-behavior
  "both first-val and second val can be signals or constants."
  [first-val second-val output-signal operator]
  {:arithmetic_conditions {(cond (map? first-val) :first_signal
                                 (int? first-val) :first_constant)
                           first-val
                           (cond (map? second-val) :second_signal
                                 (int? second-val) :second_constant)
                           second-val
                           :operation operator
                           :output_signal output-signal}})

(defn determine-wires
  [& tacs]
  (let [gensyms (->> tacs
                     (mapcat rest)
                     (filter symbol?)
                     set)
        entity#-outputs (fn [gensym]
                          (->> tacs
                               (map-indexed #(vector (last %2) (inc %1)))
                               (filter #(= gensym (% 0)))
                               (map second)))
        entity#-inputs (fn [gensym]
                         (->> tacs
                              (map-indexed #(vector (drop-last %2) (inc %1)))
                              (filter (fn [tac] (some #(= gensym %) (first tac))))
                              (map second)))]
     (->> gensyms
          (mapcat (fn [gensym] (for [input (entity#-inputs gensym)
                               output (entity#-outputs gensym)]
                           [output 3 input 1])))
          vec)))
