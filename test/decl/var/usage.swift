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

func mutableParameter(_ a : Int, h : Int, var i : Int, j: Int, g: Int) -> Int { // expected-warning {{'var' in this position is interpreted as an argument label}} {{43-46=`var`}}
  i += 1 // expected-error {{left side of mutating operator isn't mutable: 'i' is a 'let' constant}}
  var j = j
  swap(&i, &j) // expected-error {{cannot pass immutable value as inout argument: 'i' is a 'let' constant}}
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

  var mutatingProperty: Int {
    mutating get { return 0 }
    mutating set {}
  }
  var nonmutatingProperty: Int {
    nonmutating get { return 0 }
    nonmutating set {}
  }
  subscript(mutating index: Int) -> Int {
    mutating get { return 0 }
    mutating set {}
  }
  subscript(nonmutating index: Int) -> Int {
    nonmutating get { return 0 }
    nonmutating set {}
  }
}

func testStructMember() -> TestStruct {
  var x = TestStruct()  // ok
  x.property = 17
  return x
}

func testMutatingProperty_get() -> TestStruct {
  var x = TestStruct()  // ok
  _ = x.mutatingProperty
  return x
}

func testMutatingProperty_set() -> TestStruct {
  var x = TestStruct()  // ok
  x.mutatingProperty = 17
  return x
}

func testNonmutatingProperty_get() -> TestStruct {
  var x = TestStruct()  // expected-warning {{variable 'x' was never mutated; consider changing to 'let' constant}}
  _ = x.nonmutatingProperty
  return x
}

func testNonmutatingProperty_set() -> TestStruct {
  var x = TestStruct()  // expected-warning {{variable 'x' was never mutated; consider changing to 'let' constant}}
  x.nonmutatingProperty = 17
  return x
}

func testMutatingSubscript_get() -> TestStruct {
  var x = TestStruct()  // ok
  _ = x[mutating: 4]
  return x
}

func testMutatingSubscript_set() -> TestStruct {
  var x = TestStruct()  // ok
  x[mutating: 4] = 17
  return x
}

func testNonmutatingSubscript_get() -> TestStruct {
  var x = TestStruct()  // expected-warning {{variable 'x' was never mutated; consider changing to 'let' constant}}
  _ = x[nonmutating: 4]
  return x
}

func testNonmutatingSubscript_set() -> TestStruct {
  var x = TestStruct()  // expected-warning {{variable 'x' was never mutated; consider changing to 'let' constant}}
  x[nonmutating: 4] = 17
  return x
}

func testSubscript() -> [Int] {
  var x = [1,2,3] // ok
  x[1] = 27
  return x
}

func testSubscriptNeverMutated() -> [Int] {
  var x = [1,2,3] // expected-warning {{variable 'x' was never mutated; consider changing to 'let' constant}}
  _ = x[1]
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
  let other = 15  // no warning?
  var othermut = 15  // no warning
#if false
  mut = abc    // These uses prevent abc/mut from being unused/unmutated.
#endif
#if compiler(>=10.0)
  othermut = other    // This use prevents other/othermut from being unused/unmutated
#endif
}

func postfixSuppression() -> Int {
  let x = 10 // used to have warning: ... 'x' was never used...
  return 5
     #if FLAG
     .modifier(x)
     #endif
}

// same as above, but with a guard statement
func testGuardWithPoundIf(x: Int?) {
  guard let x = x else { return }

#if false
  _ = x
#endif
}

// <rdar://problem/21091625> Bogus 'never mutated' warning when protocol variable is mutated only by mutating method
protocol Fooable {
  mutating func mutFoo()
  func immutFoo()
  var mutatingProperty: Int { mutating get mutating set }
  var nonmutatingProperty: Int { nonmutating get nonmutating set }
}

func testOpenExistential(_ x: Fooable,
                         y: Fooable) {
  var x = x
  let y = y
  x.mutFoo()
  y.immutFoo()
}

func testOpenExistential_mutatingProperty_get(p: Fooable) {
  var x = p
  _ = x.mutatingProperty
}

func testOpenExistential_mutatingProperty_set(p: Fooable) {
  var x = p
  x.mutatingProperty = 4
}

func testOpenExistential_nonmutatingProperty_get(p: Fooable) {
  var x = p // expected-warning {{variable 'x' was never mutated; consider changing to 'let' constant}}
  _ = x.nonmutatingProperty
}

func testOpenExistential_nonmutatingProperty_set(p: Fooable) {
  var x = p // expected-warning {{variable 'x' was never mutated; consider changing to 'let' constant}}
  x.nonmutatingProperty = 4
}

func couldThrow() throws {}

func testFixitsInStatementsWithPatterns(_ a : Int?) {
  if var b = a,    // expected-warning {{variable 'b' was never mutated; consider changing to 'let' constant}} {{6-9=let}}
      var b2 = a {   // expected-warning {{variable 'b2' was never mutated; consider changing to 'let' constant}} {{7-10=let}}
    _ = b
    _ = b2
  }
  
  for var b in [42] { // expected-warning {{variable 'b' was never mutated; consider removing 'var' to make it constant}} {{7-11=}}
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
  if true == true, let x = a {   // expected-warning {{immutable value 'x' was never used; consider replacing with '_' or removing it}} {{20-25=_}}
  }
  if let x = a, let y = a {  // expected-warning {{immutable value 'x' was never used; consider replacing with '_' or removing it}} {{6-11=_}}
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

  // Special case, turn these into an 'is' test with optional value.
  // https://github.com/apple/swift/issues/56998

  let bb: Any? = 3
  if let bbb = bb as? Int {}  // expected-warning {{value 'bbb' was defined but never used; consider replacing with boolean test}} {{6-16=}} {{19-22=is}}
  if let bbb = (bb) as? Int {}  // expected-warning {{value 'bbb' was defined but never used; consider replacing with boolean test}} {{6-16=}} {{21-24=is}}

  func aa() -> Any? { return 1 }
  if let aaa = aa() as? Int {} // expected-warning {{value 'aaa' was defined but never used; consider replacing with boolean test}} {{6-16=}} {{21-24=is}}
  if let aaa = (aa()) as? Int {} // expected-warning {{value 'aaa' was defined but never used; consider replacing with boolean test}} {{6-16=}} {{23-26=is}}

  func bb() -> Any { return 1 }
  if let aaa = aa() as? Int {} // expected-warning {{value 'aaa' was defined but never used; consider replacing with boolean test}} {{6-16=}} {{21-24=is}}
  if let aaa = (aa()) as? Int {} // expected-warning {{value 'aaa' was defined but never used; consider replacing with boolean test}} {{6-16=}} {{23-26=is}}


  func throwingAA() throws -> Any? { return 1 }
  do {
    if let aaa = try! throwingAA() as? Int {} // expected-warning {{value 'aaa' was defined but never used; consider replacing with boolean test}} {{8-18=}} {{36-39=is}}
    if let aaa = (try! throwingAA()) as? Int {} // expected-warning {{value 'aaa' was defined but never used; consider replacing with boolean test}} {{8-18=}} {{38-41=is}}
    if let aaa = try throwingAA() as? Int {} // expected-warning {{value 'aaa' was defined but never used; consider replacing with boolean test}} {{8-18=}} {{35-38=is}}
    if let aaa = (try throwingAA()) as? Int {} // expected-warning {{value 'aaa' was defined but never used; consider replacing with boolean test}} {{8-18=}} {{37-40=is}}
  } catch { }
  if let aaa = try? throwingAA() as? Int {} // expected-warning {{value 'aaa' was defined but never used; consider replacing with boolean test}} {{6-16=(}} {{41-41=) != nil}}
  if let aaa = (try? throwingAA()) as? Int {} // expected-warning {{value 'aaa' was defined but never used; consider replacing with boolean test}} {{6-16=}} {{36-39=is}}
  
  func throwingBB() throws -> Any { return 1 }
  do { 
    if let bbb = try! throwingBB() as? Int {} // expected-warning {{value 'bbb' was defined but never used; consider replacing with boolean test}} {{8-18=}} {{36-39=is}}
    if let bbb = (try! throwingBB()) as? Int {} // expected-warning {{value 'bbb' was defined but never used; consider replacing with boolean test}} {{8-18=}} {{38-41=is}}
    if let bbb = try throwingBB() as? Int {} // expected-warning {{value 'bbb' was defined but never used; consider replacing with boolean test}} {{8-18=}} {{35-38=is}}
    if let bbb = (try throwingBB()) as? Int {} // expected-warning {{value 'bbb' was defined but never used; consider replacing with boolean test}} {{8-18=}} {{37-40=is}}
  } catch { }
  if let bbb = try? throwingBB() as? Int {} // expected-warning {{value 'bbb' was defined but never used; consider replacing with boolean test}} {{6-16=(}} {{41-41=) != nil}}
  if let bbb = (try? throwingBB()) as? Int {} // expected-warning {{value 'bbb' was defined but never used; consider replacing with boolean test}} {{6-16=}} {{36-39=is}}

  let cc: (Any?, Any) = (1, 2)
  if let (cc1, cc2) = cc as? (Int, Int) {} // expected-warning {{immutable value 'cc1' was never used; consider replacing with '_' or removing it}} expected-warning {{immutable value 'cc2' was never used; consider replacing with '_' or removing it}}

  // https://github.com/apple/swift/issues/43725

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
if let string = optionalAny as? String {} // expected-warning {{value 'string' was defined but never used; consider replacing with boolean test}} {{4-17=}} {{29-32=is}}

// Due to the complexities of global variable tracing, these will not generate warnings
let unusedVariable = ""
var unNeededVar = false
if unNeededVar {}
guard let foo = optionalAny else {}

for i in 0..<10 { // expected-warning {{immutable value 'i' was never used; consider replacing with '_' or removing it}} {{5-6=_}}
   print("")
}

// https://github.com/apple/swift/issues/45026
do {
  let x: Int // expected-warning {{immutable value 'x' was never used; consider removing it}}
  x = 42
}

// Thttps://github.com/apple/swift/issues/43576
do {
  var noOpSetter: String {
    get { return "" }
    set { } // No warning
  }
  var suspiciousSetter: String {
    get { return "" }
    set {
      print(suspiciousSetter) // expected-warning {{setter argument 'newValue' was never used, but the property was accessed}} expected-note {{did you mean to use 'newValue' instead of accessing the property's current value?}} {{13-29=newValue}}
    }
  }
  struct MemberGetterStruct {
    var suspiciousSetter: String {
      get { return "" }
      set {
        print(suspiciousSetter) // expected-warning {{setter argument 'newValue' was never used, but the property was accessed}} expected-note {{did you mean to use 'newValue' instead of accessing the property's current value?}} {{15-31=newValue}}
      }
    }
  }
  class MemberGetterClass {
    var suspiciousSetter: String {
      get { return "" }
      set {
        print(suspiciousSetter) // expected-warning {{setter argument 'newValue' was never used, but the property was accessed}} expected-note {{did you mean to use 'newValue' instead of accessing the property's current value?}} {{15-31=newValue}}
      }
    }
  }
  var namedSuspiciousSetter: String {
    get { return "" }
    set(parameter) {
      print(namedSuspiciousSetter) // expected-warning {{setter argument 'parameter' was never used, but the property was accessed}} expected-note {{did you mean to use 'parameter' instead of accessing the property's current value?}} {{13-34=parameter}}
    }
  }
  var okSetter: String {
    get { return "" }
    set { print(newValue) } // No warning
  }
  var multiTriggerSetter: String {
    get { return "" }
    set {
      print(multiTriggerSetter) // expected-warning {{setter argument 'newValue' was never used, but the property was accessed}} expected-note {{did you mean to use 'newValue' instead of accessing the property's current value?}} {{13-31=newValue}}
      print(multiTriggerSetter)
    }
  }
}
struct MemberGetterExtension {}
extension MemberGetterExtension {
  var suspiciousSetter: String {
    get { return "" }
    set {
      print(suspiciousSetter) // expected-warning {{setter argument 'newValue' was never used, but the property was accessed}} expected-note {{did you mean to use 'newValue' instead of accessing the property's current value?}} {{13-29=newValue}}
    }
  }
}

func testLocalFunc() {
  var unusedVar = 0
  // expected-warning@-1 {{initialization of variable 'unusedVar' was never used; consider replacing with assignment to '_' or removing it}}

  var notMutatedVar = 0
  // expected-warning@-1 {{variable 'notMutatedVar' was never mutated; consider changing to 'let' constant}}

  var mutatedVar = 0
  // expected-warning@-1 {{variable 'mutatedVar' was written to, but never read}}

  func localFunc() {
    _ = notMutatedVar
    mutatedVar = 1
  }
}

// False positive "'var' was never mutated" warning - rdar://60563962
func testForwardReferenceCapture() {
  func innerFunc() {
    x = 10
  }

  var x: Int = 1
  innerFunc()
  print(x)
}

// rdar://47240768 Expand the definition of "simple" pattern to variables bound in patterns
func testVariablesBoundInPatterns() {
  enum StringB {
    case simple(b: Bool)
    case tuple(b: (Bool, Bool))
    case optional(b: Bool?)
  }

  // Because Swift enables all kinds of creative binding forms, make sure that
  // variable patterns occurring directly under a `let` or `var` have that
  // introducer stripped by the fixit. All other cases are currently too
  // complicated for the VarDeclUsageChecker.
  switch StringB.simple(b: true) {
  case .simple(b: let b) where false: // expected-warning {{immutable value 'b' was never used; consider replacing with '_' or removing it}} {{19-24=_}}
    break
  case .simple(b: var b) where false: // expected-warning {{variable 'b' was never used; consider replacing with '_' or removing it}} {{19-24=_}}
    break
  case var .simple(b: b): // expected-warning {{variable 'b' was never used; consider replacing with '_' or removing it}} {{23-24=_}}
    break
  case .tuple(b: let (b1, b2)) where false:
    // expected-warning@-1 {{immutable value 'b1' was never used; consider replacing with '_' or removing it}} {{23-25=_}}
    // expected-warning@-2 {{immutable value 'b2' was never used; consider replacing with '_' or removing it}} {{27-29=_}}
    break
  case .tuple(b: (let b1, let b2)) where false:
    // expected-warning@-1 {{immutable value 'b1' was never used; consider replacing with '_' or removing it}} {{19-25=_}}
    // expected-warning@-2 {{immutable value 'b2' was never used; consider replacing with '_' or removing it}} {{27-33=_}}
    break
  case .tuple(b: (var b1, var b2)) where false:
    // expected-warning@-1 {{variable 'b1' was never used; consider replacing with '_' or removing it}} {{19-25=_}}
    // expected-warning@-2 {{variable 'b2' was never used; consider replacing with '_' or removing it}} {{27-33=_}}
    break
  case var .tuple(b: (b1, b2)) where false:
    // expected-warning@-1 {{variable 'b1' was never used; consider replacing with '_' or removing it}} {{23-25=_}}
    // expected-warning@-2 {{variable 'b2' was never used; consider replacing with '_' or removing it}} {{27-29=_}}
    break
  case .tuple(b: let b): // expected-warning {{immutable value 'b' was never used; consider replacing with '_' or removing it}} {{18-23=_}}
    break
  case .optional(b: let x?) where false: // expected-warning {{immutable value 'x' was never used; consider replacing with '_' or removing it}} {{25-26=_}}
    break
  case .optional(b: let .some(x)) where false: // expected-warning {{immutable value 'x' was never used; consider replacing with '_' or removing it}} {{31-32=_}}
    break
  case let .optional(b: x?): // expected-warning {{immutable value 'x' was never used; consider replacing with '_' or removing it}} {{25-26=_}}
    break
  case let .optional(b: .none): // expected-warning {{'let' pattern has no effect; sub-pattern didn't bind any variables}} {{8-12=}}
    break
  }
}

// https://github.com/apple/swift/issues/56998
func testUselessCastWithInvalidParam(foo: Any?) -> Int {
  class Foo { }
  if let bar = foo as? Foo { return 42 } // expected-warning {{value 'bar' was defined but never used; consider replacing with boolean test}} {{6-16=}} {{20-23=is}}
  else { return 54 }
}

// https://github.com/swiftlang/swift/issues/72811
func testEnumeratedForLoop(a: [Int]) {
  for var (b, c) in a.enumerated() {  // expected-warning {{variable 'b' was never mutated; consider changing the pattern to 'case (..., let b, ...)'}}
    c = b
    let _ = c
  }
}

// https://github.com/swiftlang/swift/issues/79555
final class A {
  var x: () -> Void {
    { [weak self] in // Used to warn: variable 'self' was written to, but never read
      #if NOT_PROCESSED
      self?.f()
      #endif
    }
  }

 func f() {}
}
