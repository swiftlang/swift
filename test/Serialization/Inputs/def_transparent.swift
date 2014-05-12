@transparent func testTransparent(`x: Bool) -> Bool {
  return x
}

@transparent func testBuiltin() -> Int {
  var y: Int = 300;
  var z = "foo"
  return y
}

@transparent func standalone_function(`x: Int, `y: Int) -> Int {
  return x
}
@transparent func curried_function(`x: Int)(y: Int) -> Int {
  return standalone_function(x: x, y: y)
}
@transparent func calls(`i: Int, `j: Int) {
  var f1 = curried_function(x: i)
  f1(y: j);
}
func foo() -> Int { return 0 }
func runced() -> Bool { return true }

func a() {}
func b() {}
func c() {}
func d() {}
func e() {}

@transparent func test_br() {
  switch foo() {
  case _ where runced():
    a()
  case _:
    b()
  }
  c()
}

enum MaybePair {
  case Neither
  case Left(Int)
  case Right(String)
  case Both(Int, String)
}
func do_switch(`u: MaybePair) {
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

struct Wrapper {
  var value: Int
  
  @transparent init(Val: Int) {
    value = Val
  }
  
  @transparent func getValue() -> Int {
    return value
  }
  
  var valueAgain: Int {
    @transparent
    get {
      return value
    }
  }
}

@transparent extension Wrapper {
  func getValueAgain() -> Int {
    return self.value
  }
}

protocol P {
  func f() -> Self
}

@class_protocol protocol CP {
  func f() -> Self
}

@transparent 
func open_existentials(`p: P, `cp: CP) {
  p.f()
  cp.f()
}
