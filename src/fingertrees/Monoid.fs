namespace Fingertrees

module Monoid =

  type IMonoid<'a> =
    abstract member mempty: 'a
    abstract member mappend: 'a -> 'a -> 'a
    //abstract member mconcat: seq<'a> -> 'a
    //default this.mconcat arr = Seq.fold this.mappend this.mempty arr

  //let mconcat arr = List.fold this.mappend this.mempty arr
