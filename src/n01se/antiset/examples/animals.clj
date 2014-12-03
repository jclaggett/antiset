(ns n01se.antiset.examples.animals
  (:require [n01se.antiset :as s]))

;; save some typing
(def C s/complement)
(def I s/intersection)
(def U s/union)
(def D s/difference)
(def S s/symetric-difference)

(def mammals #{:bear, :cow, :cat, :whale, :dolphin})
(def aquatics #{:shark, :shrimp, :whale, :dolphin})
(def land-animals (C aquatics))
(def fish (D aquatics mammals))
