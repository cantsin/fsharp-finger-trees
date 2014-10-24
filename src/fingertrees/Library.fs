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
