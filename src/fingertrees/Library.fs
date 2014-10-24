namespace fingertrees

open Monoid

/// Documentation for my library
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///
module Library =

  /// Returns 42
  ///
  /// ## Parameters
  ///  - `num` - whatever
  let hello num = 42

  type SumMonoid() =
    inherit IMonoid<int>()
    override this.mempty = 0
    override this.mappend x y = x + y

  type ProductMonoid() =
    inherit IMonoid<int>()
    override this.mempty = 1
    override this.mappend x y = x * y

  [<EntryPoint>]
  let main args =
    printfn "Arguments passed to function : %A" args
    // Return 0. This indicates success.
    0
