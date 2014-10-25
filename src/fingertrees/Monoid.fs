namespace Fingertrees

module Monoid =
  [<AbstractClass>]
  type IMonoid<'a>() =
    abstract member mempty : 'a
    abstract member mappend : 'a -> 'a -> 'a
    abstract member mconcat : seq<'a> -> 'a
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

  type ListMonoid<'a>() =
    inherit IMonoid<list<'a>>()
    override this.mempty = []
    override this.mappend x y = List.append x y
