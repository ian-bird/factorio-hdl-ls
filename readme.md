# Instructions for use
## Syntax
Factorio Hardware Description Language Lisp (fc-hdl-ls) supports a clojure-like syntax for describing the functionality of circuits.

### Arithmetic

The following arithmetic operations, accepting 2 arguments each, are supported:
> +, -, *, /, mod, bit-shift-left, bit-shift-right, bit-and, bit-or, bit-xor

An fc-hdl-ls command to multiply two numbers would look like so:
`(* 2 3)`

### Conditionals
The language also supports the following predicates:

> \>, <, =, !=, >=, <=

These can be used in conds to conditionally route data to the output of the 
expression. An example of returning 1 when x is greater than 10 would look 
like this:

`(cond (> x 1) (* 1 1))`

There's 2 things to note here:
1) x is the left argument. Due to restrictions on the behavior of decider 
combinators in factorio, the left argument must *always* be a signal. Since the 
compiler doesn't yet offer auto-boxing for constants, the compiler will currently 
generate an error if a constant in the left position is encountered.
2) the conditional consequent is a signal. This is again due to restrictions 
on what decider combinators can output. If the user attempts to return a 
constant instead, an error is returned.

Additionally, if multiple conditions are true, all of them will return and the 
overall return value will be the sum of all of them.

A more complex example could be
```
(cond (> x 1) (* 1 1)
      (= x 10) (* 1 -1))
```
This returns 0 for numbers less than 1 and 0 if x is 10.

### Function
fc-hdl-ls supports the creation of reusable functions, each expanding to a full,
independent circuit and allowing for parallel computation and expressive code. 
An example of a simple alu-type function could be the following:
```
(fn
 [op a b] 
 (cond (= op 0) (+ a b) 
       (= op 1) (- a b)
       (= op 2) (* a b)
       (= op 3) (/ a b)))
```

### Def
Def allows you to associate a name with a signal, function or a constant.
Unlike Clojure's def, Def in fc-hdl-ls is scoped, so defs within a function will
not shadow each other on repeated calls. This allows the user to use def in 
place of let.

A trivial example of creating a labeled signal could be: `(def x (* 0 0))`

or perhaps: 
```
(def square (fn [x] (* x x)))
(def sum-of-squares (fn 
                     [a b] 
                     (+ (square a)
                        (square b))))
```

### Assoc
Assoc is perhaps the most grave misnomer in fc-hdl-ls. Rather than updating a map
value, assoc is used to direct the compiler to link different outputs together.
This is a critical component to being able to create stateful systems and achieve
turing completeness in factorio. The following demonstrates its use:

```
(def mem ; memory cell, stores value on set != 0
     (fn 
      [set input]
      (do (def mem-input (cond (!= set 0) (* 1 input)))
          (def mem-output (cond (= set 0) mem-input))
          (assoc mem-output mem-input))))
```

Tying mem-input to mem-output creates a cycle, and allows the memory cell to hold
state. This is useful for memory cells, or linking components to busses, etc.

### Do
As shown in the previous example, do allows the creation of several pieces
of a circuit sequentially, and then control over the output. This offers an
alternative to nested building of circuits, and allows for assocs to be made
inside a function.

# Limitations
fc-hdl-ls doesn't support more than 36 signals on the same wire, so keep this in
mind when designing things like memory banks.

# How to use
The primary function is compile in core. It takes fc-hdl lisp as its argument,
and returns the compiled string. An example of its use is in examples.
I haven't got around to writing my lisp machine yet, but itll be in examples
once it's complete!
