@transparent public func testTransparent(#x: Bool) -> Bool {
  return x
}

@transparent public func testBuiltin() -> Int {
  var y: Int = 300;
  var z = "foo"
  return y
}

@transparent public func standalone_function(#x: Int, #y: Int) -> Int {
  return x
}
@transparent public func curried_function(#x: Int)(y: Int) -> Int {
  return standalone_function(x: x, y: y)
}
@transparent public func calls(#i: Int, #j: Int) {
  var f1 = curried_function(x: i)
  f1(y: j);
}
public func foo() -> Int { return 0 }
public func runced() -> Bool { return true }

public func a() {}
public func b() {}
public func c() {}
public func d() {}
public func e() {}

@transparent public func test_br() {
  switch foo() {
  case _ where runced():
    a()
  case _:
    b()
  }
  c()
}

public enum MaybePair {
  case Neither
  case Left(Int)
  case Right(String)
  case Both(Int, String)
}
public func do_switch(#u: MaybePair) {
  switch u {
  case .Neither:
    a()
  case .Left:
    b()
  case .Right:
    c()
  case .Both:
    d()
  }
  e()
}

public struct Wrapper {
  public var value: Int
  
  @transparent public init(Val: Int) {
    value = Val
  }
  
  @transparent public func getValue() -> Int {
    return value
  }
  
  public var valueAgain: Int {
    @transparent
    get {
      return value
    }
  }
}

@transparent public extension Wrapper {
  func getValueAgain() -> Int {
    return self.value
  }
}

public protocol P {
  func f() -> Self
}

@class_protocol public protocol CP {
  func f() -> Self
}

@transparent public
func open_existentials(#p: P, #cp: CP) {
  p.f()
  cp.f()
}
