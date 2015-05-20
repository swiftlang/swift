@inline(never) public func testNoinline(x x: Bool) -> Bool {
  return x
}

public struct NoInlineInitStruct {
  var x: Bool

  @inline(never)
  public init(x x2: Bool) {
    self.x = x2
  }
}
