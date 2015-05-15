// RUN: %target-parse-verify-swift

// <rdar://problem/20872721> QoI: warn about unused variables
// <rdar://problem/15975935> warning that you can use 'let' not 'var'
// <rdar://problem/18876585> Compiler should warn me if I set a parameter as 'var' but never modify it

func basicTests() -> Int {
  let x = 42 // expected-warning {{immutable value 'x' was never used; consider replacing with assignment to '_' or removing it}}
  var y = 12 // expected-warning {{variable 'y' was never mutated; consider changing to 'let' constant}}
  let _ = 42 // ok
  _ = 42 // ok
  return y
}

func mutableParameter(a : Int, h : Int, var i : Int, var j: Int,
       var g : Int) -> Int { // expected-warning {{parameter 'g' was never mutated; consider changing to 'let' constant}}
  swap(&i, &j)
  return i+g
}

struct X {
  func f() {}
  mutating func g() {}
}


func testStruct() {
  let a = X()
  a.f()
  
  var b = X()
  b.g()
  
  var c = X()  // expected-warning {{variable 'c' was never mutated; consider changing to 'let' constant}}
  c.f()
}

func takeClosure(fn : () -> ()) {}

class TestClass {

  func f() {
   
    takeClosure { [weak self] in  // self is mutable but never mutated.  Ok because it is weak
      self?.f()
    }
  }
}


func nestedFunction() -> Int {
  var x = 42  // No warning about being never-set.
  
  func g() {
    x = 97
    var q = 27  // expected-warning {{variable 'q' was never used}}
  }
  g()
  
  return x
}

func neverRead() {
  var x = 42  // expected-warning {{variable 'x' was written to, but never read}}
  
  x = 97
  x = 123
}

func property() -> Int {
  var p : Int {  // everything ok
    return 42
  }
  return p
}


func testInOut(inout x : Int) {  // Ok.
}

struct TestStruct {
  var property = 42
}


func testStructMember() -> TestStruct {
  var x = TestStruct()  // ok
  x.property = 17
  return x
}


func testSubscript() -> [Int] {
  var x = [1,2,3] // ok
  x[1] = 27
  return x
}


func testTuple(var x : Int) -> Int {
  var y : Int  // Ok, stored by a tuple
  
  (x, y) = (1,2)
  return y
}
  


 struct TestComputedPropertyStruct {
  
  var x : Int {
    get {}
    nonmutating set {}
  }
}

func test() {
  let v = TestComputedPropertyStruct()
  v.x = 42
  
  var v2 = TestComputedPropertyStruct()  // expected-warning {{variable 'v2' was never mutated; consider changing to 'let' constant}}
  v2.x = 42
}

func test4() {
  // expected-warning @+1 {{variable 'dest' was never mutated; consider changing to 'let' constant}}
  var dest = UnsafeMutablePointer<Int>(bitPattern: 0)

  dest[0] = 0
}

func testTuple() {
  var tup : (x:Int, y:Int)  // expected-warning {{variable 'tup' was written to, but never read}}
  tup.x = 1

  // <rdar://problem/20927707> QoI: 'variable was never mutated' noisy when only part of a destructured tuple is mutated
  var (tupA, tupB) = (1,2)  // don't warn about tupB being changable to a 'let'.
  tupA += tupB

}


/// <rdar://problem/20911927> False positive in the "variable was never mutated" warning with IUO
func testForceValueExpr() {
  var a: X! = nil  // no warning, mutated through the !
  a!.g()
}

// <rdar://problem/20894455> "variable was never mutated" diagnostic does not take #if into account
func testBuildConfigs() {
  let abc = 42    // no warning.
  var mut = 18    // no warning.
#if false
  mut = abc    // These uses prevent abc/mut from being unused/unmutated.
#endif
}


