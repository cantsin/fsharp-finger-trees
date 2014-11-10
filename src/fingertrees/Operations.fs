namespace Fingertrees

open FingerTree
open Monoid

// disable warnings for unmatched patterns in this file: we can
// trivially prove that all such matches are exhaustive.
#nowarn "25"

type Operations<'V, 'T when 'V :> IMonoid<'V> and 'T :> IMeasured<'V, 'T>>() =

  // worst case: O(lg n). amortized time: O(1)
  static member prepend (this: FingerTree<'V, 'T>) (a: 'T): FingerTree<'V, 'T> =
    match this with
      | Empty -> Single(a)
      | Single(x) ->
        Digit { annotation = fmeasure x;
                prefix = One(a);
                content = Empty;
                suffix = One(x) }
      // overflow case.
      | Digit { Finger.annotation = annotation;
                Finger.prefix = Four(p1, p2, p3, p4);
                Finger.content = content;
                Finger.suffix = suffix } ->
        let new_annotation = (fmeasure a).mappend annotation (fmeasure content)
        Digit { annotation = annotation;
                prefix = Two(a, p1);
                content = Operations.prepend content (Branch3(new_annotation, p2, p3, p4));
                suffix = suffix }
      | Digit { Finger.annotation = annotation;
                Finger.prefix = prefix;
                Finger.content = content;
                Finger.suffix = suffix } ->
        let newPrefix =
          match prefix with
            | One(x) -> Two(a, x)
            | Two(x, y) -> Three(a, x, y)
            | Three(x, y, z) -> Four(a, x, y, z) in
            Digit { annotation = annotation;
                    prefix = newPrefix;
                    content = content;
                    suffix = suffix }

  // worst case: O(lg n). amortized time: O(1)
  static member append (this: FingerTree<'V, 'T>) (a: 'T): FingerTree<'V, 'T> =
    match this with
      | Empty -> Single(a)
      | Single(x) ->
        Digit { annotation = fmeasure x;
                prefix = One(x);
                content = Empty;
                suffix = One(a) }
      // overflow case.
      | Digit { Finger.annotation = annotation;
                Finger.prefix = prefix;
                Finger.content = content;
                Finger.suffix = Four(p1, p2, p3, p4) } ->
        let new_annotation = (fmeasure a).mappend annotation (fmeasure content)
        Digit { annotation = annotation;
                prefix = prefix;
                content = Operations.append content (Branch3(new_annotation, p1, p2, p3));
                suffix = Two(p4, a) }
      | Digit { Finger.annotation = annotation;
                Finger.prefix = prefix;
                Finger.content = content;
                Finger.suffix = suffix } ->
        let newSuffix =
          match suffix with
            | One(x) -> Two(x, a)
            | Two(x, y) -> Three(x, y, a)
            | Three(x, y, z) -> Four(x, y, z, a) in
            Digit { annotation = annotation; //XXX
                    prefix = prefix;
                    content = content;
                    suffix = newSuffix }

  // helper function: convert affix to a balanced fingertree.
  static member private _affixToTree (affix: Affix<'V, 'T>): FingerTree<'V, 'T> =
    match affix with
      | One(x) -> Single(x)
      | Two(x, y) ->
        Digit { annotation = fmeasure affix;
                prefix = One(x);
                content = Empty;
                suffix = One(y) }
      // arbitrarily chosen
      | Three(x, y, z) ->
        Digit { annotation = fmeasure affix;
                prefix = One(x);
                content = Empty;
                suffix = Two(y, z) }
      // arbitrarily chosen
      | Four(x, y, z, w) ->
        Digit { annotation = fmeasure affix;
                prefix = Two(x, y);
                content = Empty;
                suffix = Two(z, w) }

  static member popl (this: FingerTree<'V, 'T>): View<'V, 'T> =
    let nodeToFinger n =
      match n with
        | Branch2(_, a, b) -> Two(a, b)
        | Branch3(_, a, b, c) -> Three(a, b, c) in
    match this with
      | Empty -> EmptyTree
      | Single(x) -> View(x, Empty)
      | Digit { Finger.annotation = annotation;
                Finger.prefix = One(x);
                Finger.content = content;
                Finger.suffix = suffix } ->
        let rest: FingerTree<'V, 'T> =
          match Operations.popl content with
            | View(inner, rest) ->
              let prefix = nodeToFinger(inner)
              let monoid = fmeasure rest
              let new_annotation = monoid.mappend (mconcat [prefix; suffix]) (fmeasure rest)
              Digit { annotation = new_annotation;
                      prefix = prefix;
                      content = rest;
                      suffix = suffix }
            | EmptyTree -> Operations._affixToTree suffix
        View(x, rest)
      | Digit { Finger.annotation = annotation;
                Finger.prefix = prefix;
                Finger.content = content;
                Finger.suffix = suffix } ->
        let l, newPrefix =
          match prefix with
            | Two(x, y) -> x, One(y)
            | Three(x, y, z) -> x, Two(y, z)
            | Four(x, y, z, w) -> x, Three(y, z, w) in
            View(l,
                 Digit { annotation = annotation; //XXX
                         prefix = newPrefix;
                         content = content;
                         suffix = suffix })

  static member popr (this: FingerTree<'V, 'T>): View<'V, 'T> =
    let nodeToFinger n =
      match n with
        | Branch2(_, a, b) -> Two(a, b)
        | Branch3(_, a, b, c) -> Three(a, b, c) in
    match this with
      | Empty -> EmptyTree
      | Single(x) -> View(x, Empty)
      | Digit { Finger.annotation = annotation;
                Finger.prefix = prefix;
                Finger.content = content;
                Finger.suffix = One(x) } ->
        let rest: FingerTree<'V, 'T> =
          match Operations.popr content with
            | View(inner, rest) ->
              let suffix = nodeToFinger(inner)
              let monoid = fmeasure rest
              let new_annotation = monoid.mappend (mconcat [prefix; suffix]) (fmeasure rest)
              Digit { annotation = new_annotation;
                      prefix = prefix;
                      content = rest;
                      suffix = suffix }
            | EmptyTree -> Operations._affixToTree prefix
        View(x, rest)
      | Digit { Finger.annotation = annotation;
                Finger.prefix = prefix;
                Finger.content = content;
                Finger.suffix = suffix } ->
        let l, newSuffix =
          match suffix with
            | Two(x, y) -> y, One(x)
            | Three(x, y, z) -> z, Two(x, y)
            | Four(x, y, z, w) -> w, Three(x, y, z) in
            View(l,
                 Digit { annotation = annotation;
                         prefix = prefix;
                         content = content;
                         suffix = newSuffix })

  static member first (this: FingerTree<'V, 'T>): Option<'T> =
    match Operations.popl this with
      | View(x, _) -> Some(x)
      | EmptyTree -> None

  static member last (this: FingerTree<'V, 'T>): Option<'T> =
    match Operations.popr this with
      | View(x, _) -> Some(x)
      | EmptyTree -> None

  static member rest (this: FingerTree<'V, 'T>): FingerTree<'V, 'T> =
    match Operations.popl this with
      | View(_, x) -> x
      | EmptyTree -> Empty

  static member butlast (this: FingerTree<'V, 'T>): FingerTree<'V, 'T> =
    match Operations.popr this with
      | View(_, x) -> x
      | EmptyTree -> Empty

  static member isEmpty (this: FingerTree<'V, 'T>): bool =
    match this with
      | Empty -> true
      | _ -> false

  // amortized O(lg(min(m,n)))
  static member private _concat (left: FingerTree<'V, 'T>)
                                (middle: list<'T>)
                                (right: FingerTree<'V, 'T>): FingerTree<'V, 'T> =
    match(left, middle, right) with
      // trivial cases.
      | Empty, [], right -> right
      | left, [], Empty -> left
      // single trees.
      | Single(y), xs, right ->
        let rightTree: FingerTree<'V, 'T> = Operations._concat Empty xs right
        Operations.prepend rightTree y
      | left, xs, Single(y) ->
        let leftTree: FingerTree<'V, 'T> = Operations._concat left xs Empty
        Operations.append leftTree y
      // joining the middle.
      | Empty, x :: xs, right ->
        let rightTree: FingerTree<'V, 'T> = Operations._concat Empty xs right
        Operations.prepend rightTree x
      | left, l, Empty ->
        // could be optimized better.
        let x = Seq.last l
        let xs = Seq.take (List.length l - 1) l |> Seq.toList
        let leftTree: FingerTree<'V, 'T> = Operations._concat left xs Empty
        Operations.append leftTree x
      // the complex case.
      | left, middle, right ->
        let (Digit leftDigit) = left
        let (Digit rightDigit) = right
        // to handle the left suffix + middle + right prefix case,
        // we convert 'em all to a list, concatenate, and then
        // reconstruct the result as nodes.
        let listify affix =
          match affix with
            | One(x) -> [x]
            | Two(x, y) -> [x; y]
            | Three(x, y, z) -> [x; y; z]
            | Four(x, y, z, w) -> [x; y; z; w]
        let monoid = fmeasure left
        let mergeAll = List.concat [listify leftDigit.suffix;
                                    middle;
                                    listify rightDigit.prefix]
        let annotation = monoid.mappend leftDigit.annotation rightDigit.annotation
        let rec listToNode l =
          match l with
            | [x; y] -> [Branch2(annotation, x, y)]
            | [x; y; z] -> [Branch3(annotation, x, y, z)]
            | x :: (y :: xs) -> [Branch2(annotation, x, y)] @ listToNode xs
        let middle' = listToNode mergeAll
        let content = Operations._concat leftDigit.content middle' rightDigit.content
        let newPrefix = leftDigit.prefix
        let newSuffix = rightDigit.suffix
        let new_annotation = monoid.mappend (fmeasure newPrefix) (fmeasure newSuffix)
        Digit { annotation = annotation;
                prefix = newPrefix;
                content = content;
                suffix = newSuffix }

  static member concat (this: FingerTree<'V, 'T>)
                       (that: FingerTree<'V, 'T>): FingerTree<'V, 'T> =
    Operations._concat this [] that

  // helper function: split list at predicate.
  static member private _splitList list predicate value =
    let monoid = fmeasure value
    // List.partition (fun item -> predicate (monoid.mappend item monoid)) list
    match list with
      | [] -> failwith "list split could not be found."
      | x :: xs ->
        let start = monoid.mappend x monoid
        if predicate start then
          ([], x::xs)
        else
          let (before, after) = Operations<'V, 'T>._splitList xs predicate start
          (x::before, after)

  // helper function: split affix into fingertree * value * fingertree.
  static member private _splitAffix affix predicate value =
    let monoid = fmeasure value
    let aot x = Operations._affixToTree x // save some typing
    let isSplit x = predicate (monoid.mappend x monoid)
    match affix with
      | One(x) when isSplit x           -> Empty,                x, Empty
      | Two(x, y) when isSplit x        -> Empty,                x, aot (One(y))
      | Two(x, y) when isSplit y        -> aot (One(x)),         y, Empty
      | Three(x, y, z) when isSplit x   -> Empty,                x, aot (Two(y, z))
      | Three(x, y, z) when isSplit y   -> aot (One(x)),         y, aot (One(z))
      | Three(x, y, z) when isSplit z   -> aot (Two(x, y)),      z, Empty
      | Four(x, y, z, w) when isSplit x -> Empty,                x, aot (Three(y, z, w))
      | Four(x, y, z, w) when isSplit y -> aot (One(x)),         y, aot (Two(z, w))
      | Four(x, y, z, w) when isSplit z -> aot (Two(x, y)),      z, aot (One(w))
      | Four(x, y, z, w) when isSplit w -> aot (Three(x, y, z)), w, Empty
      | _ -> failwith "affix split could not be found."

  static member private _affixToList a =
    match a with
      | One(x) -> [x]
      | Two(x, y) -> [x; y]
      | Three(x, y, z) -> [x; y; z]
      | Four(x, y, z, w) -> [x; y; z; w]

  static member private _nodeToList (n: Node<'V, 'T>): List<'T> =
    match n with
      | Branch2(_, x, y) -> [x; y]
      | Branch3(_, x, y, z) -> [x; y; z]

  static member private _listToAffix l =
    match l with
      | [x] -> One(x)
      | [x; y] -> Two(x, y)
      | [x; y; z] -> Three(x, y, z)
      | [x; y; z; w] -> Four(x, y, z, w)
      | _ -> failwith "invalid arg."

  // helper function: given a list, a tree, and a list, re-create a fingertree.
  static member private _digit (prefix: List<'T>)
                               (digit: FingerTree<'V, Node<'V, 'T>>)
                               (suffix: List<'T>) : FingerTree<'V, 'T> =
    match prefix, suffix with
      | [], [] ->
        match Operations<'V, Node<'V, 'T>>.popl digit with
          | EmptyTree -> Empty
          | View(item, rest) ->
            let newPrefix = Operations<'V, 'T>._nodeToList item
            Operations<'V, 'T>._digit newPrefix rest []
      | [], _ ->
        match Operations<'V, Node<'V, 'T>>.popr digit with
          | EmptyTree ->
            Operations<'V, 'T>._listToAffix suffix |> Operations._affixToTree
          | View(item, rest) ->
            let newPrefix = Operations<'V, 'T>._nodeToList item
            Operations<'V, 'T>._digit newPrefix rest suffix
      | _, [] ->
        match Operations<'V, Node<'V, 'T>>.popr digit with
          | EmptyTree ->
            Operations<'V, 'T>._listToAffix prefix |> Operations._affixToTree
          | View(item, rest) ->
            let newSuffix = Operations<'V, 'T>._nodeToList item
            Operations<'V, 'T>._digit prefix rest newSuffix
      | _ ->
        assert (List.length prefix <= 4)
        assert (List.length suffix <= 4)
        let newPrefix = Operations<'V, 'T>._listToAffix prefix
        let newSuffix = Operations<'V, 'T>._listToAffix suffix
        // XXX. fix this.
        // let annotation = mconcat [List.map fmeasure prefix |> mconcat;
        //                           fmeasure digit;
        //                           List.map fmeasure suffix |> mconcat]
        let annotation = fmeasure digit
        Digit { annotation = annotation;
                prefix = newPrefix;
                content = digit;
                suffix = newSuffix }

  static member private _chunkToTree l =
    match l with
      | [] -> Empty
      | xs -> Operations<'V, 'T>._listToAffix xs |> Operations._affixToTree

  // predicate must be monotonic.
  // value is the annotation on the left-most subsequence
  static member split (this: FingerTree<'V, 'T>)
                      (predicate: 'V -> bool)
                      (value: 'V): Split<'V, 'T> =
    if predicate value then
      failwith "predicate is always true."
    match this with
      | Single(x)
          when predicate ((fmeasure x).mappend value (fmeasure x)) ->
            Empty, x, Empty
      | Digit { Finger.annotation = annotation;
                Finger.prefix = prefix;
                Finger.content = content;
                Finger.suffix = suffix }
          when predicate (annotation.mappend value annotation) ->
          let monoid = fmeasure prefix
          let starting = monoid.mappend value monoid
          let prefixList = Operations<'V, 'T>._affixToList prefix
          let suffixList = Operations<'V, 'T>._affixToList suffix
          if predicate starting then
            // split point is in prefix.
            let (before, hit :: after) = Operations<'V, 'T>._splitList prefixList predicate value
            let newTree = Operations<'V, 'T>._digit after content suffixList
            let newBefore = Operations<'V, 'T>._chunkToTree before
            newBefore, hit, newTree
          elif mconcat [starting; (fmeasure content)] |> predicate then
            // split point is in nested tree.
            failwith "not implemented."
            // let before, hit, after = Operations<'V, 'T>.split content predicate starting
            // let newValue = mconcat [value; monoid; fmeasure before]
            // let newNode = Operations._nodeToList hit
            // let (before', hit' :: after') = Operations<'V, 'T>._splitList newNode predicate newValue
            // let prefixTree = Operations<'V, 'T>._digit prefixList before before'
            // let suffixTree = Operations<'V, 'T>._digit after' after suffixList
            // prefixTree, hit, suffixTree
          else
            // split point is in suffix.
            let newValue = monoid.mappend starting (fmeasure content)
            let (before, hit :: after) = Operations<'V, 'T>._splitList suffixList predicate newValue
            let newTree = Operations<'V, 'T>._digit prefixList content before
            let newAfter = Operations<'V, 'T>._chunkToTree after
            newTree, hit, newAfter
      | _ -> failwith "split could not be found."
