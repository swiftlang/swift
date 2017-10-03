@_inlineable
@inline(never) public func testNoinline(x x: Bool) -> Bool {
  return x
}

public struct NoInlineInitStruct {
  @_versioned
  var x: Bool

  @_inlineable
  @inline(never)
  public init(x x2: Bool) {
    self.x = x2
  }
}
