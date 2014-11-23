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

  let listToRandomAccess arr = List.fold (Operations.append) Empty arr

  let nth tree index =
    let total: Size = fmeasure tree
    if index < 0 || index >= total.Value then
      None
    else
      let (_, h, _) = Operations.splitTree tree (fun x -> x.Value > index) (Size(0))
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

  type PValue<'T> =
    { Item: 'T
      PriorityValue: int }
    interface IMeasured<Prioritized, PValue<'T>> with
      member this.fmeasure =
        Prioritized(Priority(this.PriorityValue))

  let listToPriority l =
    let accum acc x = Operations.append acc { Item = x; PriorityValue = String.length x }
    List.fold accum Empty l

  let pop pq =
    let (maxp: Prioritized) = fmeasure pq
    let nohit = Prioritized(NegativeInfinity)
    let compare (x: Prioritized) = maxp.Value = x.Value
    let (left, hit, right) = Operations.splitTree pq compare nohit
    let npq = Operations.concat left right
    hit.Item, npq

// ordered sequence on finger trees (specialization of priority queue)
module OrderedSequence =

  type OrderedKey<'T when 'T: comparison> =
    | NoKey
    | Key of 'T

  let second x y =
    match x, y with
      | (v, NoKey) -> v
      | (_, v) -> v

  [<StructuredFormatDisplay("{Value}")>]
  type Ordered<'T when 'T: comparison>(k) =
    let key = k: OrderedKey<'T>
    new() = Ordered(NoKey)
    member this.Value = key
    interface IMonoid<Ordered<'T>> with
      member this.mempty = Ordered(NoKey)
      member this.mappend x y = Ordered(second x.Value y.Value)

  type Last<'T when 'T: comparison> =
    { Last: 'T }
    interface IMeasured<Ordered<'T>, Last<'T>> with
      member this.fmeasure =
        Ordered(Key(this.Last))

  let insert seq a =
    let value = Ordered(Key(a)).Value
    let compare (x: Ordered<'T>) = x.Value >= value
    let (left, right) = Operations.split seq compare
    let right' = Operations.prepend right { Last = a }
    Operations.concat left right'

  let listToSequence l = List.fold insert Empty l

  let partition seq a =
    let value = Ordered(Key(a)).Value
    let compare (x: Ordered<'T>) = x.Value >= value
    Operations.split seq compare

  let delete seq a =
    let value = Ordered(Key(a)).Value
    let greaterThanOrEquals (x: Ordered<'T>) = x.Value >= value
    let greaterThan (x: Ordered<'T>) = x.Value > value
    let (left, right) = Operations.split seq greaterThanOrEquals
    let (left', right') = Operations.split right greaterThan
    Operations.concat left right'

  let rec merge seqA seqB =
    match Operations.popl seqB with
      | EmptyTree ->
        seqA
      | View(item, rest) ->
        let value = Ordered(Key(item.Last)).Value
        let greaterThan (x: Ordered<'T>) = x.Value > value
        let (left, right) = Operations.split seqA greaterThan
        let merged = merge rest right
        let result = Operations.prepend merged { Last = item.Last }
        Operations.concat left result

// interval trees on finger trees (uses priority queue and ordered sequence)
module IntervalTrees =

  open PriorityQueue
  open OrderedSequence

  // cannot be generalized?: tuples as generic parameters are sealed.
  [<StructuredFormatDisplay("{Value}")>]
  type ProductMonoid(p) =
    let product = p
    new() = ProductMonoid((new Ordered<int>(), new Prioritized()))
    member this.Value = product
    // access functions.
    member this.GetPriority() =
      let (_, b) = this.Value
      b.Value
    member this.GetKey() =
      let (a, _) = this.Value
      a.Value
    // monoid definition.
    interface IMonoid<ProductMonoid> with
      member this.mempty =
        ProductMonoid((new Ordered<int>(), new Prioritized()))
      member this.mappend x y =
        let (a, b) = x.Value
        let (a', b') = y.Value
        ProductMonoid(((a :> IMonoid<Ordered<int>>).mappend a a',
                       (b :> IMonoid<Prioritized>).mappend b b'))

  type Interval =
    { low: int
      high: int }
    interface IMeasured<ProductMonoid, Interval> with
      member this.fmeasure =
        ProductMonoid((Ordered(Key(this.low)), Prioritized(Priority(this.high))))

  let listToIntervalTree l =
    let accum acc (x, y) = Operations.append acc { low = x; high = y }
    List.fold accum Empty l

  // predicates.
  let atleast (product: ProductMonoid) (a: int): bool =
    Priority(a) <= product.GetPriority()

  let greater (product: ProductMonoid) (a: int): bool =
    product.GetKey() > Key(a)

  let intervalSearch tree (i: Interval) =
    let m: ProductMonoid = fmeasure tree
    let isHigher = atleast m i.low
    if not isHigher then
      None
    else
      let empty = (m :> IMonoid<ProductMonoid>).mempty
      let splitOn product = atleast product i.low
      let (_, x, _) = Operations.splitTree tree splitOn empty
      let isLower = x.low <= i.high
      if isLower then
        Some(x)
      else
        None

  let intervalMatch tree (i: Interval) =
    let m: ProductMonoid = fmeasure tree
    let isLower product = greater product i.high
    let lower = Operations.takeUntil tree isLower
    let rec matches xs =
      let isHigher product = atleast product i.low
      let higher = Operations.dropUntil xs isHigher
      let result = Operations.popl higher
      match result with
        | EmptyTree -> []
        | View(elem, rest) -> elem :: matches rest
    matches lower
