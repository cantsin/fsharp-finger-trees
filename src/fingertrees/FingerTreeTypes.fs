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
