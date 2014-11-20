namespace Fingertrees

module Monoid =

  type Singleton<'T when 'T: (new: unit -> 'T)> private () =
    static let instance = new 'T()
    static member Instance = instance

  type IMonoid<'a> =
    abstract member mempty: 'a
    abstract member mappend: 'a -> 'a -> 'a
    // abstract member mconcat: seq<'a> -> 'a
