# Finger Trees in F#

An implementation of a 2-3 finger trees data structure in F# (without laziness).

A finger tree is a data structure with first-order nested types using monoids and reductions (via generics/parametric polymorphism). The reference paper used was [Finger trees - a simple general-purpose data structure (Hinze, Paterson)](https://github.com/cantsin/fsharp-finger-trees/blob/master/reference/Finger%20trees%20-%20a%20simple%20general-purpose%20data%20structure%20(Hinze,%20Paterson).pdf?raw=true).

Implementations on top of this finger tree structure include:

- random access sequences
- priority queues
- ordered sequences
- interval trees

Performance:

- append: O(1) amortized
- prepend: O(1) amortized
- concatenation: O(log(min(m,n))); m and n are number of elements on the left and right
- split: O(log(min((m, n))) * o; m and n are sizes of the subtrees returned; o is the cost of a monoid `append` operation.
- (priority queue) find: O(log(n))
- (priority queue) find all: O(m * log(n/m))
- (ordered sequences) merge: O(m * log(n/m)); n and m are the lengths of the longer and shorter sequences

This library is feature complete, with some tests, but has not been optimized for speed (specific implementations would probably be faster).