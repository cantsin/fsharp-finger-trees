namespace Fingertrees

open System
open Monoid
open FingerTree

// random access sequence on finger trees
module RandomAccess =

  // way too complicated. TODO: simplify.
  [<StructuredFormatDisplay("{Value}")>]
  type Size(x: int) =
    let data = x
    new() = Size(0)
    member this.Value = data
    interface IMonoid<Size> with
      member this.mempty = Size(0)
      member this.mappend x y = Size(x.Value + y.Value)
    interface IComparable with
      member this.CompareTo obj =
        match obj with
        | :? Size as other -> this.Value.CompareTo(other.Value)
        | _ -> failwith "invalid comparison."

  type Value<'T> =
    | Value of 'T
    interface IMeasured<Size, Value<'T>> with
      member this.fmeasure =
        Size(1)

  let nth tree index =
    let total: Size = fmeasure tree
    if index < 0 || index >= total.Value then
      None
    else
      let (_, h, _) = Operations.split tree (fun x -> x > Size index) (Size 0)
      Some(h)

  let collapse tree =
    match Operations.popr tree with
      | EmptyTree -> []
      | View(_, ending) ->
        let lastValue = fmeasure ending
        List.map (fun x -> nth tree x) [for i in 0..lastValue.Value -> i]

module PriorityQueue =

  type Priority =
    | NegativeInfinity
    | Priority of int

  let maxPriority (x: Priority) (y: Priority) =
    match x, y with
      | (NegativeInfinity, v) -> v
      | (v, NegativeInfinity) -> v
      | (Priority(v1), Priority(v2)) -> Priority(max v1 v2)

  [<StructuredFormatDisplay("{Value}")>]
  type Prioritized(p: Priority) =
    let priority = p
    new() = Prioritized(NegativeInfinity)
    member this.Value = priority
    interface IMonoid<Prioritized> with
      member this.mempty = Prioritized(NegativeInfinity)
      member this.mappend x y = Prioritized(maxPriority x.Value y.Value)

  type Value<'T> =
    { Item: 'T
      PriorityValue: int }
    interface IMeasured<Prioritized, Value<'T>> with
      member this.fmeasure =
        Prioritized(Priority(this.PriorityValue))
