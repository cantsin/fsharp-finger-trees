module FingerTree

  type Node<'V, 'T> =
    | Branch2 of 'V * 'T * 'T
    | Branch3 of 'V * 'T * 'T * 'T

  type Affix<'T> =
    | One of 'T
    | Two of 'T * 'T
    | Three of 'T * 'T * 'T
    | Four of 'T * 'T * 'T * 'T

  type Finger<'V, 'T> = {
    annotation: 'V;
    prefix: Affix<'T>;
    content: FingerTree<'V, Node<'V, 'T>>;
    suffix: Affix<'T>;
  } and FingerTree<'V, 'T> =
  | Empty
  | Single of 'T
  | Digit of Finger<int, 'T>

  type View<'V, 'T> =
    | EmptyTree
    | View of 'T * FingerTree<'V, 'T>

  type Operations<'V, 'T>() =

    // worst case: O(lg n). amortized time: O(1)
    static member prepend (this: FingerTree<'V, 'T>) (a: 'T): FingerTree<'V, 'T> =
      match this with
        | Empty -> Single(a)
        | Single(x) ->
          Digit { annotation = 0;
                  prefix = One(a);
                  content = Empty;
                  suffix = One(x) }
        // overflow case.
        | Digit { Finger.annotation = annotation;
                  Finger.prefix = Four(p1, p2, p3, p4);
                  Finger.content = content;
                  Finger.suffix = suffix } ->
          Digit { annotation = 0;
                  prefix = Two(a, p1);
                  content = Operations.prepend content (Branch3(0, p2, p3, p4));
                  suffix = suffix }
        | Digit { Finger.annotation = annotation;
                  Finger.prefix = prefix;
                  Finger.content = content;
                  Finger.suffix = suffix } ->
          let newPrefix =
            match prefix with
              | One(x) -> Two(a, x)
              | Two(x, y) -> Three(a, x, y)
              | Three(x, y, z) -> Four(a, x, y, z)
              | Four(_) -> failwith "Invalid match." in
              Digit { annotation = 0;
                      prefix = newPrefix;
                      content = content;
                      suffix = suffix }

    // worst case: O(lg n). amortized time: O(1)
    static member append (this: FingerTree<'V, 'T>) (a: 'T): FingerTree<'V, 'T> =
      match this with
        | Empty -> Single(a)
        | Single(x) ->
          Digit { annotation = 0;
                  prefix = One(x);
                  content = Empty;
                  suffix = One(a) }
        // overflow case.
        | Digit { Finger.annotation = annotation;
                  Finger.prefix = prefix;
                  Finger.content = content;
                  Finger.suffix = Four(p1, p2, p3, p4) } ->
          Digit { annotation = 0;
                  prefix = prefix;
                  content = Operations.append content (Branch3(0, p1, p2, p3));
                  suffix = Two(p4, a) }
        | Digit { Finger.annotation = annotation;
                  Finger.prefix = prefix;
                  Finger.content = content;
                  Finger.suffix = suffix } ->
          let newSuffix =
            match suffix with
              | One(x) -> Two(x, a)
              | Two(x, y) -> Three(x, y, a)
              | Three(x, y, z) -> Four(x, y, z, a)
              | Four(_) -> failwith "Invalid match." in
              Digit { annotation = 0;
                      prefix = prefix;
                      content = content;
                      suffix = newSuffix }

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
                Digit { annotation = 0;
                        prefix = nodeToFinger(inner);
                        content = rest;
                        suffix = suffix }
              | EmptyTree ->
                match suffix: Affix<'T> with
                  | One(x) -> Single(x)
                  | Two(x, y) ->
                    Digit { annotation = 0;
                            prefix = One(x);
                            content = Empty;
                            suffix = One(y) }
                  // somewhat arbitrary
                  | Three(x, y, z) ->
                    Digit { annotation = 0;
                            prefix = Two(x, y);
                            content = Empty;
                            suffix = One(z) }
                  // somewhat arbitrary
                  | Four(x, y, z, w) ->
                    Digit { annotation = 0;
                            prefix = Two(x, y);
                            content = Empty;
                            suffix = Two(z, w) }
          View(x, rest)
        | Digit { Finger.annotation = annotation;
                  Finger.prefix = prefix;
                  Finger.content = content;
                  Finger.suffix = suffix } ->
          let l, newPrefix =
            match prefix with
              | One(_) -> failwith "Invalid match."
              | Two(x, y) -> x, One(y)
              | Three(x, y, z) -> x, Two(y, z)
              | Four(x, y, z, w) -> x, Three(y, z, w) in
              View(l,
                   Digit { annotation = 0;
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
                Digit { annotation = 0;
                        prefix = prefix;
                        content = rest;
                        suffix = nodeToFinger(inner) }
              | EmptyTree ->
                match prefix: Affix<'T> with
                  | One(x) -> Single(x)
                  | Two(x, y) ->
                    Digit { annotation = 0;
                            prefix = One(x);
                            content = Empty;
                            suffix = One(y) }
                  // somewhat arbitrary
                  | Three(x, y, z) ->
                    Digit { annotation = 0;
                            prefix = One(x);
                            content = Empty;
                            suffix = Two(y, z) }
                  // somewhat arbitrary
                  | Four(x, y, z, w) ->
                    Digit { annotation = 0;
                            prefix = Two(x, y);
                            content = Empty;
                            suffix = Two(z, w) }
          View(x, rest)
        | Digit { Finger.annotation = annotation;
                  Finger.prefix = prefix;
                  Finger.content = content;
                  Finger.suffix = suffix } ->
          let l, newSuffix =
            match suffix with
              | One(_) -> failwith "Invalid match."
              | Two(x, y) -> y, One(x)
              | Three(x, y, z) -> z, Two(x, y)
              | Four(x, y, z, w) -> w, Three(x, y, z) in
              View(l,
                   Digit { annotation = 0;
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
    static member private _concat (left: FingerTree<'V, 'T>) (middle: list<'T>) (right: FingerTree<'V, 'T>): FingerTree<'V, 'T> =
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
          let mergeAll = List.concat [listify leftDigit.suffix;
                                      middle;
                                      listify rightDigit.prefix]
          let rec listToNode l =
            match l with
              | [x; y] -> [Branch2(0, x, y)]
              | [x; y; z] -> [Branch3(0, x, y, z)]
              | x :: (y :: xs) -> [Branch2(0, x, y)] @ listToNode xs
          let middle' = listToNode mergeAll
          let content = Operations._concat leftDigit.content middle' rightDigit.content
          Digit { annotation = 0;
                  prefix = leftDigit.prefix;
                  content = content;
                  suffix = rightDigit.suffix }

    static member concat (this: FingerTree<'V, 'T>) (that: FingerTree<'V, 'T>): FingerTree<'V, 'T> =
      Operations._concat this [] that

  // shortcut operators for convenience.
  let (<||) = Operations.prepend
  let (||>) = Operations.append

  // construct a finger tree given a list.
  let toFingerTree arr = List.fold (||>) Empty arr
