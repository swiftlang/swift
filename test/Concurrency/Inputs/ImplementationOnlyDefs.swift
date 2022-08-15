open class BSuper {
  public init() { }
}

open class BSub: BSuper {
  public override init() { }
}

open class C {
  @preconcurrency open func f(_: @escaping @Sendable () -> Void) { }
  @preconcurrency open func g(_: @escaping @Sendable () -> Void) -> BSuper { BSuper() }
}
