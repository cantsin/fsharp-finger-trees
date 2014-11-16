namespace Fingertrees.Tests

open NUnit.Framework
open FsCheck
open FsCheck.NUnit

open System
open Fingertrees
open Monoid
open FingerTree
open RandomAccess

module public TestSumMonoid =

  type SumMonoid() =
//    interface IMonoid<int> with
    member this.mempty = 0
    member this.mappend x y = x + y
    member this.mconcat arr = Seq.fold this.mappend this.mempty arr

  [<TestFixtureSetUp>]
  let monoid = SumMonoid()

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
    assert(total.Value = 14)
