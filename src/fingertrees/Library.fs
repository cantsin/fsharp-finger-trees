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
  let prefix2: Affix<Node<char>> = Two(Branch2('i','s'),
                                       Branch2('i','s'));
  let suffix2: Affix<Node<char>> = Two(Branch3('n','o','t'),
                                       Branch2('a','t'));
  let suffix1: Affix<char> = Three('r','e','e');

  let content1: Finger<Node<char>> = { annotation = 1;
                                       prefix = prefix2;
                                       content = Empty;
                                       suffix = suffix2; };
  let content2: Finger<char> = { annotation = 0;
                                 prefix = prefix1;
                                 content = Digit(content1);
                                 suffix = suffix1 };
  let test_tree: FingerTree<char> = Digit(content2);

  [<EntryPoint>]
  let main args =
    let new_tree = (test_tree ||> 't' ||> 'e' ||> 's' ||> 't' ||> 'i' ||> 'n' ||> 'g')
    let new_tree2 =
      match Operations.popr new_tree with
        | View(_, result) ->
          match Operations.popr result with
            | View(_, result) -> result
    let int_tree = toFingerTree [1;2;3;4;5;6;7;8;9]
    let int_tree_doubled = toFingerTree [1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9]
    printfn "original finger tree: %A" new_tree
    printfn "modified finger tree: %A" new_tree2
    printfn "int tree: %A" int_tree
    printfn "concatenated tree: %A" (Operations.concat int_tree int_tree)
    printfn "comparison tree: %A" int_tree_doubled
    0
