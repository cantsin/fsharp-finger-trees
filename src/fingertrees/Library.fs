namespace Fingertrees

open FingerTree
open RandomAccess
open PriorityQueue

module Library =

  // shortcut operators for convenience.
  let (<||) = Operations.prepend
  let (||>) = Operations.append

  // construct a finger tree given a list.
  let toFingerTree arr = List.fold (||>) Empty arr

  [<EntryPoint>]
  let main args =
    // testing random access.
    let stringToIndex str = [for c in str -> Value c] |> toFingerTree
    let sft = stringToIndex "thisisnotatree"
    printfn "testing constructed tree: %A" sft
    let sft2 = Operations.concat sft sft
    let split = Operations.split sft2 (fun x -> x.Value > 14) (Size(1))
    printfn "split: %A" split
    let i = nth sft 0
    printfn "index 0: %A" i
    printfn "%A" (collapse (Operations.takeUntil sft (fun x -> x.Value > 5)))
    // testing a priority queue.
    let strings = ["bb"; "ffffff"; "a"; "ccc"; "eeeee"; "dddd"]
    let listToPriority l =
      let accum acc x = acc ||> { Item = x; PriorityValue = String.length x }
      List.fold accum Empty l
    let pft = listToPriority strings
    printfn "testing constructed tree: %A" pft
    let (longest, q) = pop pft
    printfn "%A" longest
    let (longest, q) = pop q
    printfn "%A" longest
    let (longest, q) = pop q
    printfn "%A" longest
    let (longest, q) = pop q
    printfn "%A" longest
    0
