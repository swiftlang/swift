open class Base {
  public init() {}

  internal func method() -> Int {
    return 1
  }
}

open class Middle : Base {
  open override func method() -> Int {
    return super.method() + 1
  }
}

public func callBaseMethod(_ b: Base) -> Int {
  return b.method()
}

public func callMiddleMethod(_ m: Middle) -> Int {
  return m.method()
}
