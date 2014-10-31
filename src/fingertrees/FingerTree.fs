namespace Fingertrees

open Monoid

module FingerTree =

  type IMeasured<'V, 'T when 'V :> IMonoid<'V>> =
    abstract member fmeasure: 'V

  type Node<'V, 'T when 'V :> IMonoid<'V> and 'T :> IMeasured<'V, 'T>> =
    | Branch2 of 'V * 'T * 'T
    | Branch3 of 'V * 'T * 'T * 'T
    interface IMeasured<'V, Node<'V, 'T>> with
      member this.fmeasure: 'V =
        match this with
          | Branch2(v, _, _) -> v
          | Branch3(v, _, _, _) -> v

  type Affix<'V, 'T when 'V :> IMonoid<'V> and 'T :> IMeasured<'V, 'T>> =
    | One of 'T
    | Two of 'T * 'T
    | Three of 'T * 'T * 'T
    | Four of 'T * 'T * 'T * 'T
    interface IMeasured<'V, 'T> with
      member this.fmeasure: 'V =
        let getMeasure m = (m :> IMeasured<'V, 'T>).fmeasure
        match this with
          | One(x) -> getMeasure(x)
          | Two(x, y) ->
            let m1 = (x :> IMeasured<'V, 'T>).fmeasure
            let m2 = (y :> IMeasured<'V, 'T>).fmeasure
            m1.mappend m1 m2
          | Three(x, y, z) ->
            let m1 = (x :> IMeasured<'V, 'T>).fmeasure
            let m2 = (y :> IMeasured<'V, 'T>).fmeasure
            let m3 = (z :> IMeasured<'V, 'T>).fmeasure
            m1.mappend (m1.mappend m1 m2) m3
          | Four(x, y, z, w) ->
            let m1 = (x :> IMeasured<'V, 'T>).fmeasure
            let m2 = (y :> IMeasured<'V, 'T>).fmeasure
            let m3 = (z :> IMeasured<'V, 'T>).fmeasure
            let m4 = (w :> IMeasured<'V, 'T>).fmeasure
            m1.mappend (m1.mappend (m1.mappend m1 m2) m3) m4

  type Finger<'V, 'T when 'V :> IMonoid<'V> and 'T :> IMeasured<'V, 'T>> = {
    annotation: 'V;
    prefix: Affix<'V, 'T>;
    content: FingerTree<'V, Node<'V, 'T>>;
    suffix: Affix<'V, 'T>;
  }

  and FingerTree<'V, 'T when 'V :> IMonoid<'V> and 'T :> IMeasured<'V, 'T>> =
    | Empty
    | Single of 'T
    | Digit of Finger<'V, 'T>
    interface IMeasured<'V, 'T> with
      member this.fmeasure: 'V =
        match this with
        | Empty -> (this :> IMeasured<'V, 'T>).fmeasure.mempty
        | Single(x) -> (x :> IMeasured<'V, 'T>).fmeasure
        | Digit(d) -> d.annotation

  type View<'V, 'T when 'V :> IMonoid<'V> and 'T :> IMeasured<'V, 'T>> =
    | EmptyTree
    | View of 'T * FingerTree<'V, 'T>

  // // construct a finger tree given a list.
  // let toFingerTree arr = List.fold (||>) Empty arr
