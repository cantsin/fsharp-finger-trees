namespace Fingertrees

open FingerTree
open IntervalTrees

module Library =

  // shortcut operators for convenience.
  let (<||) = Operations.prepend
  let (||>) = Operations.append

  [<EntryPoint>]
  let main args =
    let listToIntervalTree l =
      let accum acc (x, y) = acc ||> { low = x; high = y }
      List.fold accum Empty l
    let it = listToIntervalTree [(1,2); (2,3); (3,4)]
    printfn "%A" it
    let hit = intervalSearch it { low = 10; high = 20 }
    printfn "%A" hit
    let hits = intervalMatch it { low = 3; high = 4 }
    printfn "%A" hits
    0
