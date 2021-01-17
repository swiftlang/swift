public struct Mystruct {
  var x: Int

  public init(_ x: Int) { self.x = x }
}

@inline(never)
@inlinable
public func testParam<T>(_ t: T) {
  print(t)
}

@inline(never)
@inlinable
public func testReturn<T>(_ a: [T]) -> T {
  return a[0]
}

public func otherFunc() {
  testParam(Mystruct(27))
  print(testReturn([Mystruct(28)]))
}

