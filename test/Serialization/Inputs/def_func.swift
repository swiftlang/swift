public func getZero() -> Int {
  return 0
}

public func getInput(#x: Int) -> Int {
  return x
}

public func getSecond(Int, #y: Int) -> Int {
  return y
}

public func useNested((x: Int, y: Int), #n: Int) {}

public func variadic(#x: Double, y: Int...) {}

public func slice(#x: [Int]) {}
public func optional(#x: Int?) {}

public func overloaded(#x: Int) {}
public func overloaded(#x: Bool) {}

// Generic functions.
public func makePair<A, B>(#a: A, #b: B) -> (A, B) {
  return (a, b)
}

public func different<T : Equatable>(#a: T, #b: T) -> Bool {
  return a != b
}

public func different2<T where T : Equatable>(#a: T, #b: T) -> Bool {
  return a != b
}

public func selectorFunc1(#a: Int, b x: Int) {}

public protocol Wrapped {
  typealias Value : Equatable
  
  //var value : Value
  func getValue() -> Value
}

public func differentWrapped<
  T : Wrapped, U : Wrapped
  where
  T.Value == U.Value
>(#a: T, #b: U) -> Bool {
  return a.getValue() != b.getValue()
}

@noreturn @asmname("exit") public func exit ()->()

@noreturn public func testNoReturnAttr() -> () { exit() }
@noreturn public func testNoReturnAttrPoly<T>(#x: T) -> () { exit() }


@asmname("primitive") public func primitive()

public protocol EqualOperator {
  func ==(x: Self, y: Self) -> Bool
}

public func throws1() throws {}
public func throws2<T>(t: T) -> T { return t }