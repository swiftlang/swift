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

extension OuterNonGeneric.InnerGeneric {
  public typealias AliasTy = (Y1, Y2)
}

extension OuterGeneric.InnerNonGeneric {
  public typealias AliasTy = (X1, X2)
}

extension OuterGeneric.InnerGeneric {
  public typealias AliasTy = (X1, X2, Y1, Y2)
}
