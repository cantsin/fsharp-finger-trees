namespace Fingertrees

module Monoid =
  [<AbstractClass>]
  type IMonoid<'a>() =
    abstract member mempty : 'a
    abstract member mappend : 'a -> 'a -> 'a
    abstract member mconcat : seq<'a> -> 'a
    default this.mconcat arr = Seq.fold this.mappend this.mempty arr
