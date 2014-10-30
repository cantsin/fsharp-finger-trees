namespace Fingertrees

module Monoid =
  [<AbstractClass>]
  type IMonoid<'T>() =
    abstract member mempty : 'T
    abstract member mappend : 'T -> 'T -> 'T
    abstract member mconcat : seq<'T> -> 'T
    default this.mconcat arr = Seq.fold this.mappend this.mempty arr

  // some implementations.
  type SumMonoid() =
    inherit IMonoid<int>()
    override this.mempty = 0
    override this.mappend x y = x + y

  type ProductMonoid() =
    inherit IMonoid<int>()
    override this.mempty = 1
    override this.mappend x y = x * y

  type ListMonoid<'V>() =
    inherit IMonoid<list<'V>>()
    override this.mempty = []
    override this.mappend x y = List.append x y
