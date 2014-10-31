namespace Fingertrees

module Monoid =

  type IMonoid<'V> =
    abstract member mempty: 'V
    abstract member mappend: 'V -> 'V -> 'V
    //abstract member mconcat: seq<'V> -> 'V
    //default this.mconcat arr = Seq.fold this.mappend this.mempty arr

  //let mconcat arr = List.fold this.mappend this.mempty arr
