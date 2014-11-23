# fsharp-finger-trees

An implementation of a 2-3 finger trees data structure in F# (without laziness).

A finger tree is a data structure with first-order nested types using monoids and reductions (via generics/parametric polymorphism). The reference paper used was [Finger trees - a simple general-purpose data structure (Hinze, Paterson)](reference/Finger trees - a simple general-purpose data structure (Hinze, Paterson).pdf).

Implementations on top of this finger tree structure include:

- random access sequence
- a priority queue
- an ordered sequence
- an interval tree

This library is feature complete, with a testing framework, but has not been optimized for speed (specific implementations would probably be faster).