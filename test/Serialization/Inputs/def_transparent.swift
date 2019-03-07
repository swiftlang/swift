@_transparent public func testTransparent(x x: Bool) -> Bool {
  return x
}

@_transparent public func testBuiltin() -> Int32 {
  var y: Int32 = 300
  var z = "foo"
  return y
}

@_transparent public func standalone_function(x x: Int32, y: Int32) -> Int32 {
  return x
}

@inlinable
public func foo() -> Int32 { return 0 }
@inlinable
public func runced() -> Bool { return true }

@inlinable
public func a() {}
@inlinable
public func b() {}
@inlinable
public func c() {}
@inlinable
public func d() {}
@inlinable
public func e() {}

@_transparent public func test_br() {
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

@_transparent
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

@_fixed_layout
public struct Wrapper {
  public var value: Int32
  
  @_transparent public init(Val: Int32) {
    value = Val
  }
  
  @_transparent public func getValue() -> Int32 {
    return value
  }
  
  public var valueAgain: Int32 {
    @_transparent
    get {
      return value
    }
  }
}

public extension Wrapper {
  @_transparent
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

@_transparent public
func open_existentials(p p: P, cp: CP) {
  p.f()
  cp.f()
}
