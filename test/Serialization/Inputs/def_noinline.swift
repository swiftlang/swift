@inlinable
@inline(never) public func testNoinline(x x: Bool) -> Bool {
  return x
}

@_fixed_layout
public struct NoInlineInitStruct {
  @usableFromInline
  var x: Bool

  @inlinable
  @inline(never)
  public init(x x2: Bool) {
    self.x = x2
  }
}
