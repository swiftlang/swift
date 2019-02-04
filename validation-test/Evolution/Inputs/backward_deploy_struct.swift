public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

#if AFTER
@_weakLinked public struct ResilientStruct {
  public init() {}

  public func fn(_ x: Int) {}

  public var storedProp: Int = 0

  public var computedProp: Int {
    get { return 0 }
    set { }
  }

  public subscript(idx: Int) -> Int {
    get { return 0 }
    set { }
  }
}

@_weakLinked @_fixed_layout public struct FixedLayoutStruct {
  public init() {}

  public func fn(_ x: Int) {}

  public var storedProp: Int = 0

  public var computedProp: Int {
    get { return 0 }
    set { }
  }

  public subscript(idx: Int) -> Int {
    get { return 0 }
    set { }
  }
}
#endif
