(ns scratchpad
  (:require 
   [blueprint-serialization :as bs]))

(->> "data/example_blueprint.txt"
     slurp
     bs/to-sexpr
     (spit "data/example_blueprint.edn"))