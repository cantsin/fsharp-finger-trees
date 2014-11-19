namespace Fingertrees

open FingerTree
open OrderedSequence

module Library =

  // shortcut operators for convenience.
  let (<||) = Operations.prepend
  let (||>) = Operations.append

  [<EntryPoint>]
  let main args =
    // testing an ordered sequence.
    let listToSequence l =
      let accum acc x = acc ||> { Last = x }
      List.fold accum Empty l
    let oft = listToSequence [1..20]
    printfn "testing constructed ordered sequence: %A" oft
    let v = Ordered(Key(16))
    let result = partition oft v
    printfn "test: %A" result
    0
