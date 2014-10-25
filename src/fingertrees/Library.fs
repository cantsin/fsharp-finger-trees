namespace Fingertrees

open Monoid

type Node<'a> =
  | Branch2 of 'a * 'a
  | Branch3 of 'a * 'a * 'a

type Affix<'a> =
  | One of 'a
  | Two of 'a * 'a
  | Three of 'a * 'a * 'a
  | Four of 'a * 'a * 'a * 'a

// TODO restrict V to monoid
type Finger<'V, 'T> = {
  annotation: 'V;
  prefix: Affix<'T>;
  content: FingerTree<'V, Node<'T>>;
  suffix: Affix<'T>;
} and FingerTree<'V, 'T> =
  | Empty        // special case: empty tree
  | Single of 'T // special case: tree with one element
  | Digit of Finger<'V, 'T>

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

  let content1: Finger<int, Node<char>> = { annotation = 1;
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
    printfn "sample finger tree: %A" test_tree
    0
