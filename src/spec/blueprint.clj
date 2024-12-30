(ns spec.blueprint
  (:require 
   [spec.shorthand :as shorthand]))

(defn signal [] '(or {:name string?} {:type string? :name string?}))

(shorthand/specdef
 :fc-bp
 {:blueprint
  {:icons [{:signal (signal) :index pos-int?} +]
   :entities [(or {:entity_number pos-int?
                   :name string?
                   :position {:x number? :y number?}}
                  {:entity_number pos-int?
                   :name string?
                   :position {:x int? :y int?}
                   :control_behavior
                   (or {:arithmetic_conditions {:first_signal (signal)
                                                :second_signal (signal)
                                                :operation string?
                                                :output_signal (signal)}}
                       {:decider_conditions
                        {:conditions [(or {:first_signal (signal)
                                           :second_signal (signal)
                                           :compartor string?}
                                          {:first_signal (signal)
                                           :constant int?
                                           :comparator string?}) +]
                         :outputs [{:signal (signal)
                                    :copy_count_from_input boolean?} *]}}
                       {:operation string?
                        :select_max boolean?
                        :index_constant int?})}) +]
   :wires [[pos-int? +] +]
   :item string?
   :version pos-int?}})