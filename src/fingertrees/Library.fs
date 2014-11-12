namespace Fingertrees

open Monoid
open System
open FingerTree

module Library =

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

  // shortcut operators for convenience.
  let (<||) = Operations.prepend
  let (||>) = Operations.append

  // TODO independent of Size?
  let index tree idx =
    if idx < 0 then
      // TODO annotation on Finger
      None
    else
      let (_, h, _) = Operations.split tree (fun x -> x > Size idx) (Size 0)
      Some(h)

  // construct a finger tree given a list or string.
  let toFingerTree arr = List.fold (||>) Empty arr
  // TODO independent of Value?
  let stringToFingerTree str = [for c in str -> Value c] |> toFingerTree

  let sft = stringToFingerTree "thisisnotatree"

  // test an example tree by hand
  let prefix1: Affix<Size, Value<char>> =
    Two(Value('t'),Value('h'))
  let prefix2: Affix<Size, Node<Size, Value<char>>> =
      Two(Branch2(Size(2),
                  Value('i'),Value('s')),
          Branch2(Size(2),
                  Value('i'),Value('s')));
  let suffix2: Affix<Size, Node<Size, Value<char>>> =
    Two(Branch3(Size(3),
                Value('n'),Value('o'),Value('t')),
        Branch2(Size(2),
                Value('a'),Value('t')));
  let suffix1: Affix<Size, Value<char>> =
    Three(Value('r'),Value('e'),Value('e'));

  let content1: Finger<Size, Node<Size, Value<char>>> =
      { annotation = Size(9);
        prefix = prefix2;
        content = Empty;
        suffix = suffix2; };
  let content2: Finger<Size, Value<char>> =
    { annotation = Size(14);
      prefix = prefix1;
      content = Digit(content1);
      suffix = suffix1 };
  let testTree: FingerTree<Size, Value<char>> =
    Digit(content2);

  [<EntryPoint>]
  let main args =
    // let newTree = (testTree ||> 't' ||> 'e' ||> 's' ||> 't' ||> 'i' ||> 'n' ||> 'g')
    // let intTree = toFingerTree [1;2;3;4;5;6;7;8;9]
    // let intTreeDoubled = toFingerTree [1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9]
    // printfn "original finger tree: %A" newTree
    // printfn "modified finger tree: %A" newTree2
    // printfn "int tree: %A" intTree
    // printfn "concatenated tree: %A" (Operations.concat intTree intTree)
    // printfn "comparison tree: %A" intTreeDoubled
    printfn "testing monadic tree: %A" testTree
    printfn "testing string tree: %A" sft
    // let secondary =
    //   match Operations.popr sft with
    //     | View(_, result) ->
    //       match Operations.popr result with
    //         | View(_, result) ->
    //         match Operations.popr result with
    //           | View(_, result) ->
    //             match Operations.popr result with
    //               | View(_, result) ->
    //               match Operations.popr result with
    //                 | View(_, result) -> result
    // printfn "secondary: %A" secondary
    let sft2 = Operations.concat sft sft
    printfn "sft2: %A" sft2

    // let i = index sft 0
    // printfn "index 0: %A" i
    0
