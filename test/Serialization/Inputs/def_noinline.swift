@inline(never) public func testNoinline(#x: Bool) -> Bool {
  return x
}
@inline(late) public func testNoinlineLate(#x: Bool) -> Bool {
  return x
}
public struct NoInlineInitStruct {
  var x: Bool

  @inline(never)
  public init(x x2: Bool) {
    self.x = x2
  }
}
public struct LateInlineInitStruct {
  var x: Bool

  @inline(late)
  public init(x x2: Bool) {
    self.x = x2
  }
}
