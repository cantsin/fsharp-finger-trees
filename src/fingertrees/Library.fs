namespace Fingertrees

open Monoid
open System
open FingerTree

/// Documentation for my library
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///
module Library =

  // test an example tree by hand
  let prefix1: Affix<char> = Two('t','h')
  let prefix2: Affix<Node<int, char>> = Two(Branch2(1, 'i','s'),
                                            Branch2(2, 'i','s'));
  let suffix2: Affix<Node<int, char>> = Two(Branch3(3, 'n','o','t'),
                                            Branch2(4, 'a','t'));
  let suffix1: Affix<char> = Three('r','e','e');

  let content1: Finger<int, Node<int, char>> = { annotation = 1;
                                                 prefix = prefix2;
                                                 content = Empty;
                                                 suffix = suffix2; };
  let content2: Finger<int, char> = { annotation = 0;
                                      prefix = prefix1;
                                      content = Digit(content1);
                                      suffix = suffix1 };
  let test_tree: FingerTree<int, char> = Digit(content2);

  [<EntryPoint>]
  let main args =
    let new_tree = (test_tree |> 't' |> 'e' |> 's' |> 't' |> 'i' |> 'n' |> 'g')
    let new_tree2 = match Operations.popl new_tree with
      | View(_, result) -> match Operations.popl result with
        | View(_, result) -> result
    printfn "sample finger tree: %A" new_tree2
    0
