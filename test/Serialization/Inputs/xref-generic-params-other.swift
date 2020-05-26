public struct OuterNonGeneric {
  public struct InnerNonGeneric {
    public typealias AliasTy = ()
  }
  public struct InnerGeneric<Y1, Y2> {
    public typealias AliasTy = (Y1, Y2)
  }
}

public struct OuterGeneric<X1, X2> {
  public struct InnerNonGeneric {
    public typealias AliasTy = (X1, X2)
  }
  public struct InnerGeneric<Y1, Y2> {
    public typealias AliasTy = (X1, X2, Y1, Y2)
  }
}
