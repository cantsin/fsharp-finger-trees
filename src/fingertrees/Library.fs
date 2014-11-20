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
    printfn "test: %A" result
    0
