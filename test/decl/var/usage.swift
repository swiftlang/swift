// RUN: %target-parse-verify-swift

// <rdar://problem/20872721> QoI: warn about unused variables
// <rdar://problem/15975935> warning that you can use 'let' not 'var'
// <rdar://problem/18876585> Compiler should warn me if I set a parameter as 'var' but never modify it

func basicTests() -> Int {
  let x = 42 // expected-warning {{immutable value 'x' was never used; consider replacing with assignment to '_' or removing it}} {{3-8=_}}
  var y = 12 // expected-warning {{variable 'y' was never mutated; consider changing to 'let' constant}} {{3-6=let}}
  _ = 42 // ok
  _ = 42 // ok
  return y
}

// expected-warning@+2 {{Use of 'var' binding here is deprecated and will be removed in a future version of Swift}} {{41-45=}}
// expected-warning@+1 {{Use of 'var' binding here is deprecated and will be removed in a future version of Swift}} {{54-58=}}
func mutableParameter(a : Int, h : Int, var i : Int, var j: Int, 
       var g : Int) -> Int { // expected-warning {{Use of 'var' binding here is deprecated and will be removed in a future version of Swift}} {{8-12=}}
  g += 1
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
  
  var c = X()  // expected-warning {{variable 'c' was never mutated; consider changing to 'let' constant}} {{3-6=let}}
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
    var q = 27  // expected-warning {{variable 'q' was never used}} {{5-10=_}}
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


func testTuple(var x : Int) -> Int { // expected-warning {{Use of 'var' binding here is deprecated and will be removed in a future version of Swift}} {{16-19=}}
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
  
  var v2 = TestComputedPropertyStruct()  // expected-warning {{variable 'v2' was never mutated; consider changing to 'let' constant}} {{3-6=let}}
  v2.x = 42
}

func test4() {
  // expected-warning @+1 {{variable 'dest' was never mutated; consider changing to 'let' constant}} {{3-6=let}}
  var dest = UnsafeMutablePointer<Int>(bitPattern: 0)

  dest[0] = 0
}

func testTuple() {
  var tup : (x:Int, y:Int)  // expected-warning {{variable 'tup' was written to, but never read}}
  tup.x = 1

  // <rdar://problem/20927707> QoI: 'variable was never mutated' noisy when only part of a destructured tuple is mutated
  var (tupA, tupB) = (1,2)  // don't warn about tupB being changeable to a 'let'.
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

// <rdar://problem/21091625> Bogus 'never mutated' warning when protocol variable is mutated only by mutating method
protocol Fooable {
  mutating func mutFoo()
  func immutFoo()
}
func testOpenExistential(var x: Fooable, // expected-warning {{Use of 'var' binding here is deprecated and will be removed in a future version of Swift}} {{26-29=}}
                         y: Fooable) {
  x.mutFoo()
  y.immutFoo()
}


func couldThrow() throws {}

func testFixitsInStatementsWithPatterns(a : Int?) {
  // FIXME: rdar://problem/23378003
  // This will eventually be an error.
  if var b = a,    // expected-warning {{Use of 'var' binding here is deprecated and will be removed in a future version of Swift}} {{6-9=let}}
      var b2 = a {  // expected-warning {{Use of 'var' binding here is deprecated and will be removed in a future version of Swift}} {{7-10=let}}
    b = 1
    b2 = 1
    _ = b
    _ = b2
  }

  var g = [1,2,3].generate()
  // FIXME: rdar://problem/23378003
  // This will eventually be an error.
  while var x = g.next() { // expected-warning {{Use of 'var' binding here is deprecated and will be removed in a future version of Swift}} {{9-12=let}}
    x = 0
    _ = x
  }

  // FIXME: rdar://problem/23378003
  // This will eventually be an error.
  guard var y = Optional.Some(1) else { // expected-warning {{Use of 'var' binding here is deprecated and will be removed in a future version of Swift}} {{9-12=let}}
    return
  }
  y = 0
  _ = y

  // FIXME: rdar://problem/23378003
  // This will eventually be an error.
  for var b in [42] {   // expected-warning {{Use of 'var' binding here is deprecated and will be removed in a future version of Swift}} {{7-11=}}
    b = 42
    _ = b
  }

  for let b in [42] {   // expected-error {{'let' pattern is already in an immutable context}} {{7-11=}}
    _ = b
  }

  do {
    try couldThrow()
  // FIXME: rdar://problem/23378003
  // This will eventually be an error.
  } catch var err {  // expected-warning {{Use of 'var' binding here is deprecated and will be removed in a future version of Swift}} {{11-14=let}}
    // expected-warning@-1 {{variable 'err' was never mutated; consider changing to 'let' constant}}
    _ = err
  }

  switch a {
    // FIXME: rdar://problem/23378003
    // This will eventually be an error.
    case var b: // expected-warning {{Use of 'var' binding here is deprecated and will be removed in a future version of Swift}} {{10-13=let}}
      // expected-warning@-1 {{variable 'b' was never mutated; consider changing to 'let' constant}}
      _ = b
  }
}


// <rdar://22774938> QoI: "never used" in an "if let" should rewrite expression to use != nil
func test(a : Int?, b : Any) {
  if true == true, let x = a {   // expected-warning {{immutable value 'x' was never used; consider replacing with '_' or removing it}} {{24-25=_}}
  }
  if let x = a, y = a {  // expected-warning {{immutable value 'x' was never used; consider replacing with '_' or removing it}} {{10-11=_}}
    _ = y
  }

  // Simple case, insert a comparison with nil.
  if let x = a {  // expected-warning {{value 'x' was defined but never used; consider replacing with boolean test}} {{6-14=}} {{15-15= != nil}}
  }

  // General case, need to insert parentheses.
  if let x = a ?? a {}  // expected-warning {{value 'x' was defined but never used; consider replacing with boolean test}} {{6-14=(}} {{20-20=) != nil}}
  
  // Special case, we can turn this into an 'is' test.
  if let x = b as? Int {  // expected-warning {{value 'x' was defined but never used; consider replacing with boolean test}} {{6-14=}} {{16-19=is}}
  }

  
}


