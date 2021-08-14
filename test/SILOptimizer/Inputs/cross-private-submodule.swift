
public struct PrivateStr {
  var i: Int

  public init(i: Int) {
    self.i = i
  }

  @inline(never)
  public func test() -> Int {
    return i
  }
}

