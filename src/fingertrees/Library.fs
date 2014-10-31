namespace Fingertrees

open Monoid
open System
open FingerTree

module Library =

  type Size =
    | Size of int
    interface IMonoid<Size> with
      member this.mempty = Size(0)
      member this.mappend (Size(x)) (Size(y)) = Size(x + y)

  type Value<'T> =
    | Value of 'T
    interface IMeasured<Size, Value<'T>> with
      member this.fmeasure =
        Size(1)

  // test an example tree by hand
  let prefix1: Affix<Size, Value<char>> =
    Two(Value('t'),Value('h'))
  let prefix2: Affix<Size, Node<Size, Value<char>>> =
      Two(Branch2(Size(1),
                  Value('i'),Value('s')),
          Branch2(Size(2),
                  Value('i'),Value('s')));
  let suffix2: Affix<Size, Node<Size, Value<char>>> =
    Two(Branch3(Size(3),
                Value('n'),Value('o'),Value('t')),
        Branch2(Size(4),
                Value('a'),Value('t')));
  let suffix1: Affix<Size, Value<char>> =
    Three(Value('r'),Value('e'),Value('e'));

  let content1: Finger<Size, Node<Size, Value<char>>> =
      { annotation = Size(1);
        prefix = prefix2;
        content = Empty;
        suffix = suffix2; };
  let content2: Finger<Size, Value<char>> =
    { annotation = Size(0);
      prefix = prefix1;
      content = Digit(content1);
      suffix = suffix1 };
  let test_tree: FingerTree<Size, Value<char>> =
    Digit(content2);

  [<EntryPoint>]
  let main args =
    // let new_tree = (test_tree ||> 't' ||> 'e' ||> 's' ||> 't' ||> 'i' ||> 'n' ||> 'g')
    // let new_tree2 =
    //   match Operations.popr new_tree with
    //     | View(_, result) ->
    //       match Operations.popr result with
    //         | View(_, result) -> result
    // let int_tree = toFingerTree [1;2;3;4;5;6;7;8;9]
    // let int_tree_doubled = toFingerTree [1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9]
    // printfn "original finger tree: %A" new_tree
    // printfn "modified finger tree: %A" new_tree2
    // printfn "int tree: %A" int_tree
    // printfn "concatenated tree: %A" (Operations.concat int_tree int_tree)
    // printfn "comparison tree: %A" int_tree_doubled
    printfn "testing monadic tree: %A" test_tree
    0
