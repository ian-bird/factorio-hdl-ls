(ns to-tac 
  (:require
   [clojure.walk :as walk]
   [keyword-modifiers :as km]))

(def ^:dynamic *tac-statements* nil)

(defmacro eval-with-tac-append
  [names]
  (let [;create the fc and tac names and associate them
        fc->tac-names (map (fn [name] [(symbol (str "fc" name))
                                       (symbol (str "tac" name))])
                           (eval names))
        ; create a function definition for each pair of
        ; fc and tac names
        each-expanded
        (map (fn [[fc-name tac-name]]
               `(defn ~(symbol (km/unqualify fc-name))
                  [~'l ~'r]
                  (let [~'output (gensym)]
                    (set! *tac-statements*
                          (conj *tac-statements*
                                (list (quote ~tac-name) ~'l ~'r ~'output)))
                    ~'output)))
             fc->tac-names)]
    ; output in a do so all the function defs are loaded
    `(do ~@each-expanded)))

(def op-map
  {'+ 'fc+
   '- 'fc-
   '* 'fc*
   '/ 'fc-div
   'modulo 'fc-modulo
   'bit-shift-left 'fc-bit-shift-left
   'bit-shift-right 'fc-bit-shift-right
   'bit-and 'fc-bit-and
   'bit-or 'fc-bit-or
   'bit-xor 'fc-bit-xor
   '> 'fc>
   '< 'fc<
   '= 'fc=
   '!= 'fc!=
   '>= 'fc>=
   '<= 'fc<=
   'def 'fc-def
   'cond 'fc-cond
   'fn 'fc-fn
   'assoc 'fc-assoc
   'do 'fc-do})

(eval-with-tac-append (map #(symbol (apply str (drop 2 (str %))))
                           (vals op-map)))

; this needs to be a macro since we're replacing function calls before
; evaluation
(defmacro fc-def [name body]
  `(def ~name ~(walk/prewalk-replace op-map body)))

; this needs to be a macro since we need to shuffle arguments
; around so they evaluate in the right order for the function call.
(defmacro fc-cond
  [& conditions]
  `(fc-cond-fn ~@(->> conditions
                      (partition 2 2)
                      (mapcat reverse))))

(defn fc-cond-fn [& conditions]
  (let [
        antecent->consequent (->> conditions
                                  (partition 2 2)
                                  (mapcat reverse)
                                  (apply hash-map))
        output (gensym)
        new-tac (mapv (fn [ts]
                          ; looking for tac statements that have outputs
                          ; matching our antecedents
                          ;
                          ; when we find them we need to replace these with
                          ; our output wire, and insert the consequent output
                          ; as the pass-through.
                        (if (and (= 4 (count ts))
                                 (contains? antecent->consequent (last ts)))
                          (let [tsv (vec ts)]
                            (list (tsv 0)
                                  (tsv 1)
                                  (tsv 2)
                                  (antecent->consequent (last ts))
                                  output))
                          ts))
                      *tac-statements*)]
    (set! *tac-statements* new-tac)
    output))

; needs to be a macro since we're replacing calls before
; making them
(defmacro fc-fn [args body]
  `(fn [~@args] ~(walk/prewalk-replace op-map body)))

(defn fc-assoc
  [def1 def2]
  (let [; replace all occurences of def2's wire value with def1's,
        ; consolidating to a single wire.
        new-tac (mapv (fn [ts] (map #(if (= % def2) def1 %) ts))
                      *tac-statements*)]
    (set! *tac-statements* new-tac)
    def2))

(defn fc-do
  [& statements]
  (last statements))

; this needs to be a macro since we don't want to evaluate
; arguments passed to it. We're doing pretty advanced code
; replacement and capture.
(defmacro fc-lisp->tac
  [& fc-lisp-statements]
  ; we're using a temp namespace to prevent fc-def defmacros
  ; cluttering up the main namespace
  (let [old-ns (ns-name *ns*)
        temp-ns (gensym)]
    (binding [*tac-statements* []
              *ns* temp-ns]
      (in-ns temp-ns)
      (ns temp-ns
        (:require [to-tac :refer :all]))
      ; doall to force side effects on *tac-stmts*
      (doall (->> fc-lisp-statements
                  ; need to remap the operations so that the correct macros
                  ; are called
                  (map (partial walk/prewalk-replace op-map))
                  (map eval)))
      (in-ns old-ns)
      (remove-ns temp-ns)
      ;return output
      `(quote ( ~@*tac-statements*)))))

(fc-lisp->tac (def square (fn [x](* x x))) (+ (square 3) 16))
