public enum E {
  case strong

  @_weakLinked
  case weak
}

public func getVersion() -> Int {
  // Used to return 0.
  return 1
}

@_weakLinked public struct ResilientStruct {
  public init() {}

  public func fn(_ x: Int) {}
}
