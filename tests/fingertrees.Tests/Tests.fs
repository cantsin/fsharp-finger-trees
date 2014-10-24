namespace Fingertrees.Tests

open Fingertrees
open System
open FsCheck
open FsCheck.NUnit
open NUnit.Framework

[<TestFixture>]
module public TestClass =

  [<TestFixtureSetUp>]
  let foo = 5

  [<Test>]
  let ``hello returns 42`` () =
    let result = Library.hello 42
    printfn "%i" result
    Assert.AreEqual(42,result)

  [<Test>]
  let ``When 2 is added to 2 expect 4``() =
    Assert.AreEqual(4, 2+2)
