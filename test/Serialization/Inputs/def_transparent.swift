@transparent public func testTransparent(x x: Bool) -> Bool {
  return x
}

@transparent public func testBuiltin() -> Int32 {
  var y: Int32 = 300;
  var z = "foo"
  return y
}

@transparent public func standalone_function(x x: Int32, y: Int32) -> Int32 {
  return x
}
@transparent public func curried_function(x x: Int32)(y: Int32) -> Int32 {
  return standalone_function(x: x, y: y)
}
@transparent public func calls(i i: Int32, j: Int32) {
  var f1 = curried_function(x: i)
  f1(y: j);
}
public func foo() -> Int32 { return 0 }
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
  case Left(Int32)
  case Right(String)
  case Both(Int32, String)
}
public func do_switch(u u: MaybePair) {
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
  public var value: Int32
  
  @transparent public init(Val: Int32) {
    value = Val
  }
  
  @transparent public func getValue() -> Int32 {
    return value
  }
  
  public var valueAgain: Int32 {
    @transparent
    get {
      return value
    }
  }
}

@transparent public extension Wrapper {
  func getValueAgain() -> Int32 {
    return self.value
  }
}

public protocol P {
  func f() -> Self
}

public protocol CP : class {
  func f() -> Self
}

@transparent public
func open_existentials(p p: P, cp: CP) {
  p.f()
  cp.f()
}
