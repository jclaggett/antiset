(ns n01se.antiset
  "Define Antisets and various set operations that accept both anti- and
  regular sets."
  (:require [clojure.set :as s])
  (:refer-clojure :exclude [complement]))

(use 'clojure.repl)

(defprotocol Complement
  (complement [s] "Return the complement of s (U \\ s)"))

;; marker protocol.
(defprotocol Anti "A value defined in terms of what it is not" )

(deftype AntiSet [^clojure.lang.IPersistentSet non-elems]
  clojure.lang.IPersistentSet
  (contains [_ e] (not (contains? non-elems e)))
  (disjoin [_ e] (AntiSet. (conj non-elems e)))
  (cons [_ e] (AntiSet. (disj non-elems e)))

  (seq [_] (throw "Unable to seq an antiset (infinitely large)"))
  (count [_] (throw "Unable to count an antiset (infinitely large)"))
  ;; sigh. Count returns an integer so I can't return POSITIVE_INFINITY.
  #_(count [_] Double/POSITIVE_INFINITY)

  (equiv [_ other] (and (instance? AntiSet other)
                        (= non-elems (.non-elems other))))

  clojure.lang.IFn
  (invoke [_ x] (when-not (contains? non-elems x) x))

  Complement
  (complement [_] non-elems)

  Anti)

(extend-protocol Complement
  clojure.lang.IPersistentSet 
  (complement [s] (AntiSet. s))
  clojure.lang.Fn ;; wish I could use this on IFn but I can't :-(
  (complement [f] (clojure.core/complement f)))

(comment
  "These extend-types are semantically equal to the above extend-protocol."
  (extend-type clojure.lang.IPersistentSet
    Complement
    (complement [s] (AntiSet. s)))

  (extend-type clojure.lang.Fn
    Complement
    (complement [f] clojure.core/complement f)))

;; define s-* operators that only work with positive sets and pretend they are
;; a part of the clojure.set library (since they should be)
(defn s-intersect?
  "Does set1 intersect set2?"
  [set1 set2]
  (or (if (< (count set1) (count set2))
        (some #(contains? set2 %) set1)
        (some #(contains? set1 %) set2))
      false))

(defn s-disjoint?
  "Is set1 disjoint from set2?"
  [set1 set2]
  (not (s-intersect? set1 set2)))

;; Convenience macro for defining the behavior of various combinations
;; of positive and negative sets.
(defmacro case-sets [s1 s2 c1 c2 c3 c4]
  `(case [(satisfies? Anti ~s1)
          (satisfies? Anti ~s2)]
     [false false] ~c1 ;; both sets are normal 
     [false true ] ~c2
     [true  false] ~c3
     [true  true ] ~c4)) ;; both sets are anti

(defn intersect?
  "Is at least one element of set1 shared with set2?"
  [set1 set2]
  (case-sets set1 set2
    (s-intersect? set1 set2)
    (not (s/subset? set1 (complement set2)))
    (not (s/subset? set2 (complement set1)))
    true)) ;; always intersect on finite computers

(defn disjoint?
  "Are no elements in set1 shared with set2?"
  [set1 set2]
  (not (intersect? set1 set2)))

(defn subset? [set1 set2]
  "Are all elements in set1 also in set2?"
  (case-sets set1 set2
    (s/subset? set1 set2)
    (not (s-intersect? set1 (complement set2)))
    false ;; never a subset on finite computers
    (s/subset? (complement set2) (complement set1))))

(defn superset?
  "Are all elements in set2 also in set1?"
  [set1 set2]
  (subset? set2 set1))

(defn union
  "Return a set containing elements in all sets."
  [& sets]
  (reduce
   (fn
     ([] nil)
     ([set1 set2]
        (if (= (complement #{}) set1)
          (reduced set1)
          (case-sets set1 set2
            (s/union set1 set2)
            (complement (s/difference (complement set2) set1))
            (complement (s/difference (complement set1) set2))
            (complement (s/intersection (complement set1) (complement set2)))))))
   sets))

(defn intersection
  "Return a set containing only elements that are members of all sets."
  [& sets]
  (reduce
   (fn
     ([] nil)
     ([set1 set2]
        (if (= #{} set1)
          (reduced set1)
          (case-sets set1 set2
            (s/intersection set1 set2)
            (s/difference set1 (complement set2))
            (s/difference set2 (complement set1))
            (complement (s/union (complement set1) (complement set2)))))))
   sets))

(defn difference
  "Return a set containing elements of the first set that are not also elements
  in the following sets."
  [& sets]
  (reduce
   (fn
     ([] nil)
     ([set1 set2]
        (if (= #{} set1)
          (reduced set1)
          (case-sets set1 set2
            (s/difference set1 set2)
            (s/intersection set1 (complement set2))
            (complement (s/union (complement set1) set2))
            (s/difference (complement set2) (complement set1))))))
   sets))

;; Define other set operators for completeness
(defn symetric-difference
  "Return a set of elements that are found in set1 or in set2 but not in both."
  [set1 set2]
  (difference (union set1 set2)
              (intersection set1 set2)))

(defn proper-subset?
  [set1 set2]
  (and (not= set1 set2)
       (subset? set1 set2)))

(defn proper-superset? [set1 set2]
  (proper-subset? set2 set1))

;; Define print-method for AntiSet.

(defmethod print-method AntiSet [x w]
  (binding [*out* w]
    (print (apply str (concat ["#-{"]
                              (interpose " " (.non-elems x))
                              ["}"])))))

;; Define set notation. Because Unicode.

(def ∁ complement)
(def ∪ union)
(def ∩ intersection)
(def ∖ difference)
(def ⊖ symetric-difference)
(def △ symetric-difference)
(def ∅ #{})
(def ⊆ subset?)
(def ⊇ superset?)
(def ⊂ proper-subset?)
(def ⊃ proper-superset?)
(def ∋ contains?)
(def U (∁ ∅))

