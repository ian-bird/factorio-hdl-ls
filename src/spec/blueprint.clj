(ns spec.blueprint
  (:require 
   [spec.shorthand :as shorthand]))

(shorthand/specdef
  :fc-bp
  {:blueprint {:icons [{:signal {:name string?} :index pos-int?} +]
               :entities [(or {:entity_number pos-int?
                               :name string?
                               :position {:x number? :y number?}}
                              {:entity_number pos-int?
                               :name string?
                               :position {:x int? :y int?}
                               :control_behavior
                               (or {:arithmetic_conditions
                                    {:first_signal {:type string? :name string?}
                                     :second_signal {:type string? :name string?}
                                     :operation string?
                                     :output_signal {:type string? :name string?}}}
                                   {:decider_conditions
                                    {:conditions [{:first_signal {:type string?
                                                                  :name string?}
                                                   :comparator string?} *]
                                     :outputs [{:signal {:type string? :name string?}
                                                :copy_count_from_input boolean?} *]}}
                                   {:operation string?
                                    :select_max boolean?
                                    :index_constant int?})}) +]
               :wires [[pos-int? +] +]
               :item string?
               :version pos-int?}})