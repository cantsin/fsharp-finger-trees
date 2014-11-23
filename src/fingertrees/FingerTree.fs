namespace Fingertrees

open Monoid

module FingerTree =

  type IMeasured<'V, 'T when 'V :> IMonoid<'V>> =
    abstract member fmeasure: 'V

  // monoid handling functions via IMeasured
  let fmeasure m = (m :> IMeasured<'V, 'T>).fmeasure
  let mconcat (list: List<'V>) =
    match list with
      | [] -> failwith "empty."
      | [x] -> fmeasure x
      | (x :: xs) ->
      let monoid = fmeasure x
      List.map fmeasure xs |>
      List.fold monoid.mappend monoid

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
        match this with
          | One(x) -> mconcat [x]
          | Two(x, y) -> mconcat [x; y]
          | Three(x, y, z) -> mconcat [x; y; z]
          | Four(x, y, z, w) -> mconcat [x; y; z; w]

  type Finger<'V, 'T when 'V :> IMonoid<'V> and 'T :> IMeasured<'V, 'T> and 'V: (new: unit -> 'V)> = {
    annotation: 'V;
    prefix: Affix<'V, 'T>;
    content: FingerTree<'V, Node<'V, 'T>>;
    suffix: Affix<'V, 'T>;
  }

  and FingerTree<'V, 'T when 'V :> IMonoid<'V> and 'T :> IMeasured<'V, 'T> and 'V: (new: unit -> 'V)> =
    | Empty
    | Single of 'T
    | Digit of Finger<'V, 'T>
    interface IMeasured<'V, 'T> with
      member this.fmeasure: 'V =
        match this with
        | Empty -> this.monoid.mempty
        | Single(x) -> fmeasure x
        | Digit(d) -> d.annotation
    member this.monoid: 'V = Singleton<'V>.Instance

  type View<'V, 'T when 'V :> IMonoid<'V> and 'T :> IMeasured<'V, 'T> and 'V: (new: unit -> 'V)> =
    | EmptyTree
    | View of 'T * FingerTree<'V, 'T>

  type Split<'V, 'T when 'V :> IMonoid<'V> and 'T :> IMeasured<'V, 'T> and 'V: (new: unit -> 'V)> =
    FingerTree<'V, 'T> * FingerTree<'V, 'T>
