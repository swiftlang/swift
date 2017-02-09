// RUN: %target-typecheck-verify-swift

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

func mutableParameter(_ a : Int, h : Int, var i : Int, j: Int, g: Int) -> Int { // expected-error {{parameters may not have the 'var' specifier}}
  i += 1
  var j = j
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

func takeClosure(_ fn : () -> ()) {}

class TestClass {

  func f() {
   
    takeClosure { [weak self] in  // self is mutable but never mutated.  Ok because it is weak
      self?.f()
    }
  }
}

enum TestEnum {
  case Test(Int, Int, Int)
}

func testEnum() -> Int {
  let ev = TestEnum.Test(5, 6, 7)
  switch ev {
  case .Test(var i, var j, var k): // expected-warning {{variable 'i' was never mutated; consider changing to 'let' constant}} {{14-17=let}}
                                   // expected-warning@-1 {{variable 'j' was never mutated; consider changing to 'let' constant}} {{21-24=let}}
                                   // expected-warning@-2 {{variable 'k' was never mutated; consider changing to 'let' constant}} {{28-31=let}}
    return i + j + k
  default:
    return 0
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


func testInOut(_ x : inout Int) {  // Ok.
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


func testTuple(_ x : Int) -> Int {
  var x = x
  var y : Int  // Ok, stored by a tuple
  
  (x, y) = (1,2)
  _ = x
  _ = y
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
  var dest = UnsafeMutablePointer<Int>(bitPattern: 0)!

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
func testOpenExistential(_ x: Fooable,
                         y: Fooable) {
  var x = x
  let y = y
  x.mutFoo()
  y.immutFoo()
}


func couldThrow() throws {}

func testFixitsInStatementsWithPatterns(_ a : Int?) {
  if var b = a,    // expected-warning {{variable 'b' was never mutated; consider changing to 'let' constant}} {{6-9=let}}
      var b2 = a {   // expected-warning {{variable 'b2' was never mutated; consider changing to 'let' constant}} {{7-10=let}}
    _ = b
    _ = b2
  }
  
  for var b in [42] {   // expected-warning {{variable 'b' was never mutated; consider changing to 'let' constant}} {{7-10=let}}
    _ = b
  }

  do {
    try couldThrow()
  } catch var err {  // expected-warning {{variable 'err' was never mutated; consider changing to 'let' constant}} {{11-14=let}}
    _ = err
  }

  switch a {
    case var b: _ = b  // expected-warning {{variable 'b' was never mutated; consider changing to 'let' constant}} {{10-13=let}}
  }
}

// <rdar://22774938> QoI: "never used" in an "if let" should rewrite expression to use != nil
func test(_ a : Int?, b : Any) {
  if true == true, let x = a {   // expected-warning {{immutable value 'x' was never used; consider replacing with '_' or removing it}} {{24-25=_}}
  }
  if let x = a, let y = a {  // expected-warning {{immutable value 'x' was never used; consider replacing with '_' or removing it}} {{10-11=_}}
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

  // SR-1112

  let xxx: Int? = 0

  if let yyy = xxx { } // expected-warning{{with boolean test}} {{6-16=}} {{19-19= != nil}}

  var zzz: Int? = 0
  zzz = 1

  if let yyy = zzz { } // expected-warning{{with boolean test}} {{6-16=}} {{19-19= != nil}}

  if let yyy = zzz ?? xxx { } // expected-warning{{with boolean test}} {{6-16=(}} {{26-26=) != nil}}

}

func test2() {
  let a = 4 // expected-warning {{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}} {{3-8=_}}
  var ( b ) = 6 // expected-warning {{initialization of variable 'b' was never used; consider replacing with assignment to '_' or removing it}} {{3-12=_}}
  var c: Int = 4 // expected-warning {{variable 'c' was never used; consider replacing with '_' or removing it}} {{7-8=_}}
  let (d): Int = 9 // expected-warning {{immutable value 'd' was never used; consider replacing with '_' or removing it}} {{8-9=_}}
}

let optionalString: String? = "check"
if let string = optionalString {}  // expected-warning {{value 'string' was defined but never used; consider replacing with boolean test}} {{4-17=}} {{31-31= != nil}}

let optionalAny: Any? = "check"
if let string = optionalAny as? String {} // expected-warning {{value 'string' was defined but never used; consider replacing with boolean test}} {{4-17=(}} {{39-39=) != nil}}

// Due to the complexities of global variable tracing, these will not generate warnings
let unusedVariable = ""
var unNeededVar = false
if unNeededVar {}
guard let foo = optionalAny else {}

for i in 0..<10 { // expected-warning {{immutable value 'i' was never used; consider replacing with '_' or removing it}} {{5-6=_}}
   print("")
}
