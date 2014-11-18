namespace Fingertrees

open System
open Monoid
open FingerTree

// random access sequence on finger trees
module RandomAccess =

  [<StructuredFormatDisplay("{Value}")>]
  type Size(x: int) =
    let data = x
    new() = Size(0)
    member this.Value = data
    interface IMonoid<Size> with
      member this.mempty = Size(0)
      member this.mappend x y = Size(x.Value + y.Value)

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
      let (_, h, _) = Operations.split tree (fun x -> x.Value > index) (Size(0))
      Some(h)

  let collapse tree =
    match Operations.popr tree with
      | EmptyTree -> []
      | View(_, ending) ->
        let lastValue = fmeasure ending
        List.map (fun x -> nth tree x) [for i in 0..lastValue.Value -> i]

// priority queue on finger trees
module PriorityQueue =

  type Priority =
    | NegativeInfinity
    | Priority of int

  let maxPriority x y =
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

  let pop pq =
    let (maxp: Prioritized) = fmeasure pq
    let nohit = Prioritized(NegativeInfinity)
    let compare (x: Prioritized) = maxp.Value = x.Value
    let (left, hit, right) = Operations.split pq compare nohit
    let npq = Operations.concat left right
    hit.Item, npq

// ordered sequence on finger trees (specialization of priority queue)
module OrderedSequence =

  type OrderedKey<'T> =
    | NoKey
    | Key of 'T

  let second x y =
    match x, y with
      | (v, NoKey) -> v
      | (_, v) -> v

  [<StructuredFormatDisplay("{Value}")>]
  type Ordered<'T>(k) =
    let key = k: OrderedKey<'T>
    new() = Ordered(NoKey)
    member this.Value = key
    interface IMonoid<Ordered<'T>> with
      member this.mempty = Ordered(NoKey)
      member this.mappend x y = Ordered(second x.Value y.Value)

  type Last<'T> =
    { Last: 'T }
    interface IMeasured<Ordered<'T>, Last<'T>> with
      member this.fmeasure =
        Ordered(Key(this.Last))
