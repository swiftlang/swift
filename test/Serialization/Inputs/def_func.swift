public func getZero() -> Int {
  return 0
}

public func getInput(x: Int) -> Int {
  return x
}

public func getSecond(_: Int, y: Int) -> Int {
  return y
}

public func useNested(_: (x: Int, y: Int), n: Int) {}

public func variadic(x: Double, _ y: Int...) {}
public func variadic2(_ y: Int..., x: Double) {}

public func slice(x: [Int]) {}
public func optional(x: Int?) {}

public func overloaded(x: Int) {}
public func overloaded(x: Bool) {}

// Generic functions.
public func makePair<A, B>(a: A, b: B) -> (A, B) {
  return (a, b)
}

public func different<T : Equatable>(a: T, b: T) -> Bool {
  return a != b
}

public func different2<T>(a: T, b: T) -> Bool where T : Equatable {
  return a != b
}

public func selectorFunc1(a: Int, b x: Int) {}

public protocol Wrapped {
  associatedtype Value : Equatable
  
  //var value : Value
  func getValue() -> Value
}

public func differentWrapped<
  T : Wrapped, U : Wrapped
>(a: T, b: U) -> Bool
  where T.Value == U.Value
{
  return a.getValue() != b.getValue()
}

@_silgen_name("exit") public func exit () -> Never

public func testNoReturnAttr() -> Never { exit() }
public func testNoReturnAttrPoly<T>(x: T) -> Never { exit() }


@_silgen_name("primitive") public func primitive()

public protocol EqualOperator {
  static func ==(x: Self, y: Self) -> Bool
}

public func throws1() throws {}
public func throws2<T>(_ t: T) throws -> T { return t }
