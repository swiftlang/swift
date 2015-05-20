@inline(__always) public func testAlwaysInline(x x: Bool) -> Bool {
  return x
}

public struct AlwaysInlineInitStruct {
  var x: Bool

  @inline(__always)
  public init(x x2: Bool) {
    self.x = x2
  }
}

