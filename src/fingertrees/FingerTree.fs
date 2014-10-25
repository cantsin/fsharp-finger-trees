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

  type Operations<'V, 'T>() =

    // worst case: O(lg n). amortized time: O(1)
    static member prepend (this: FingerTree<int, 'T>) (a: 'T): FingerTree<'V, 'T> =
      match this with
        | Empty -> Single(a)
        | Single(y) ->
          Digit { annotation = 0;
                  prefix = One(a);
                  content = Empty;
                  suffix = One(y) }
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
          let new_prefix =
            match prefix with
              | One(x) -> Two(a, x)
              | Two(x, y) -> Three(a, x, y)
              | Three(x, y, z) -> Four(a, x, y, z)
              | Four(_) -> failwith "Invalid match." in
              Digit { annotation = 0;
                      prefix = new_prefix;
                      content = content;
                      suffix = suffix }

    // worst case: O(lg n). amortized time: O(1)
    static member append (this: FingerTree<int, 'T>) (a: 'T): FingerTree<'V, 'T> =
      match this with
        | Empty -> Single(a)
        | Single(y) ->
          Digit { annotation = 0;
                  prefix = One(y);
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
          let new_suffix =
            match suffix with
              | One(x) -> Two(x, a)
              | Two(x, y) -> Three(x, y, a)
              | Three(x, y, z) -> Four(x, y, z, a)
              | Four(_) -> failwith "Invalid match." in
              Digit { annotation = 0;
                      prefix = prefix;
                      content = content;
                      suffix = new_suffix }

  let (<|) = Operations.prepend
  let (|>) = Operations.append
