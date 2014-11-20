namespace Fingertrees.Tests

open NUnit.Framework
open FsCheck
open FsCheck.NUnit

open System
open Fingertrees
open Monoid
open FingerTree
open RandomAccess
open PriorityQueue
open OrderedSequence

module public FingerTreeTests =

  type SumMonoid() =
    member this.mempty = 0
    member this.mappend x y = x + y
    member this.mconcat arr = Seq.fold this.mappend this.mempty arr

  [<TestFixtureSetUp>]
  let monoid = SumMonoid()
  // shortcut operators for convenience.
  let (<||) = Operations.prepend
  let (||>) = Operations.append

  [<Test>]
  let ``mempty <> x = x`` () =
    Check.QuickThrowOnFailure(
      fun (x: int) ->
        monoid.mappend monoid.mempty x = x)

  [<Test>]
  let ``x <> (y <> z) = (x <> y) <> z`` () =
    Check.QuickThrowOnFailure(
      fun (x: int, y: int, z: int) ->
        monoid.mappend (monoid.mappend x y) z = monoid.mappend x (monoid.mappend y z))

  [<Test>]
  let ``moncat x, y, z = (x <> y) <> z`` () =
    Check.QuickThrowOnFailure(
      fun (x: int, y: int, z: int) ->
        monoid.mconcat [|x; y; z|] = monoid.mappend (monoid.mappend x y) z)

  [<Test>]
  let ``testing example tree by hand`` () =
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
    let total: Size = fmeasure testTree
    Assert.That(total.Value, Is.EqualTo(14))

  [<Test>]
  let ``testing random access`` () =
    let toFingerTree arr = List.fold (||>) Empty arr
    let stringToIndex str = [for c in str -> Value c] |> toFingerTree
    let sft = stringToIndex "thisisnotatree"
    let sft2 = Operations.concat sft sft
    let _, split, _ = Operations.splitTree sft2 (fun x -> x.Value > 14) (Size(1))
    Assert.That(split, Is.EqualTo(Value('e')))
    let i = nth sft 0
    Assert.That(i, Is.EqualTo(Some(Value('t'))))
    let result = collapse (Operations.takeUntil sft (fun x -> x.Value > 5))
    Assert.That(result.[0], Is.EqualTo(Some(Value('t'))))
    Assert.That(result.[1], Is.EqualTo(Some(Value('h'))))
    Assert.That(result.[2], Is.EqualTo(Some(Value('i'))))
    Assert.That(result.[3], Is.EqualTo(Some(Value('s'))))

  [<Test>]
  let ``testing a priority queue`` () =
    let strings = ["bb"; "ffffff"; "a"; "ccc"; "eeeee"; "dddd"]
    let listToPriority l =
      let accum acc x = acc ||> { Item = x; PriorityValue = String.length x }
      List.fold accum Empty l
    let pft = listToPriority strings
    let (longest, q) = pop pft
    Assert.That(longest, Is.EqualTo("ffffff"))
    let (longest, q) = pop q
    Assert.That(longest, Is.EqualTo("eeeee"))
    let (longest, q) = pop q
    Assert.That(longest, Is.EqualTo("dddd"))
    let (longest, q) = pop q
    Assert.That(longest, Is.EqualTo("ccc"))

  [<Test>]
  let ``testing an ordered sequence`` () =
    let listToSequence l = List.fold insert Empty l
    let oft = listToSequence [1..20]
    let left, right = partition oft 16
    Assert.That((fmeasure left).Value, Is.EqualTo(Key(15)))
    Assert.That((fmeasure right).Value, Is.EqualTo(Key(20)))
    let deleted = delete oft 5
    Assert.That((fmeasure deleted).Value, Is.EqualTo(Key(20)))
    let (seqA, _) = partition oft 5
    let (_, seqB) = partition oft 15
    let merged = merge seqA seqB
    Assert.That((fmeasure merged).Value, Is.EqualTo(Key(20)))
