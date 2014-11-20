namespace Fingertrees

open FingerTree
open OrderedSequence

module Library =

  // shortcut operators for convenience.
  let (<||) = Operations.prepend
  let (||>) = Operations.append

  [<EntryPoint>]
  let main args =
    let listToSequence l = List.fold insert Empty l
    let oft = listToSequence [1..20]
    printfn "testing constructed ordered sequence: %A" oft
    let result = partition oft 16
    printfn "partitioned: %A" result
    let deleted = delete oft 5
    printfn "deleted: %A" deleted
    let (seqA, _) = partition oft 5
    let (_, seqB) = partition oft 15
    let merged = merge seqA seqB
    printfn "merged: %A" merged
    0
