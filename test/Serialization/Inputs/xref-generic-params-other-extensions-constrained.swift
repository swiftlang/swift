public struct OuterNonGeneric {}
extension OuterNonGeneric {
  public struct InnerNonGeneric {}
  public struct InnerGeneric<Y1, Y2> {}
}

public struct OuterGeneric<X1, X2> {}
extension OuterGeneric {
  public struct InnerNonGeneric {}
  public struct InnerGeneric<Y1, Y2> {}
}


extension OuterNonGeneric.InnerNonGeneric {
  public typealias AliasTy = ()
}

extension OuterNonGeneric.InnerGeneric where Y1: Equatable {
  public typealias AliasTy = (Y1, Y2)
}

extension OuterGeneric.InnerNonGeneric where X1: Equatable {
  public typealias AliasTy = (X1, X2)
}

extension OuterGeneric.InnerGeneric where X1: Equatable, Y1: Equatable {
  public typealias AliasTy = (X1, X2, Y1, Y2)
}
