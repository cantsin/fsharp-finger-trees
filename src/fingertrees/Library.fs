namespace Fingertrees

open FingerTree
open RandomAccess

module Library =

  // shortcut operators for convenience.
  let (<||) = Operations.prepend
  let (||>) = Operations.append

  // construct a finger tree given a list.
  let toFingerTree arr = List.fold (||>) Empty arr

  [<EntryPoint>]
  let main args =
    // testing a random access finger tree.
    let stringToFingerTree str = [for c in str -> Value c] |> toFingerTree
    let sft = stringToFingerTree "thisisnotatree"
    printfn "testing constructed tree: %A" sft
    let sft2 = Operations.concat sft sft
    let split = Operations.split sft2 (fun x -> x > Size 14) (Size 1)
    printfn "split: %A" split
    let i = nth sft 0
    printfn "index 0: %A" i
    printfn "%A" (collapse (Operations.takeUntil sft (fun x -> x > Size 5)))
    0
