// RUN: %target-swift-frontend -emit-sil %s -verify

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

//func mutableParameter(_ a : Int, h : Int, var i : Int, j: Int, g: Int) -> Int { // FIXME: move exxpected-warning {{'var' in this position is interpreted as an argument label}} {{43-46=`var`}}
//  i += 1 // FIXME: move exxpected-error {{left side of mutating operator isn't mutable: 'i' is a 'let' constant}}
//  var j = j
//  swap(&i, &j) // FIXME: move exxpected-error {{cannot pass immutable value as inout argument: 'i' is a 'let' constant}}
//  return i+g
//}

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
  default: // expected-warning {{default will never be executed}}
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
    get {} // expected-error {{missing return in a function expected to return 'Int'}}
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
  // don't warn about tupB being changeable to a 'let'.
  var (tupA, tupB) = (1,2)  // expected-warning {{variable 'tupA' was written to, but never read}}
  tupA += tupB

}


/// <rdar://problem/20911927> False positive in the "variable was never mutated" warning with IUO
func testForceValueExpr() {
  var a: X! = nil  // no warning, mutated through the !
  a!.g()
}

// FIXME: delete?
//// <rdar://problem/20894455> "variable was never mutated" diagnostic does not take #if into account
//func testBuildConfigs() {
//  let abc = 42    // no warning.
//  var mut = 18    // no warning.
//#if false
//  mut = abc    // These uses prevent abc/mut from being unused/unmutated.
//#endif
//}
//
//// same as above, but with a guard statement
//func testGuardWithPoundIf(x: Int?) {
//  guard let x = x else { return }
//
//#if false
//  _ = x
//#endif
//}

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
  
  var z: Int // expected-warning {{variable 'z' was written to, but never read}}
  if let x = a, let y = a {  // expected-warning {{immutable value 'x' was never used; consider replacing with '_' or removing it}} {{6-11=_}}
    z = y
  }
  
  // FIXME: Impossible to notice `_ = y` in SILGen, do we ignore it?
//  if let x = a, let y = a {  // exxpected-warning {{immutable value 'x' was never used; consider replacing with '_' or removing it}} {{6-11=_}}
//    _ = y
//  }

  // Simple case, insert a comparison with nil.
  if let x = a {  // expected-warning {{value 'x' was defined but never used; consider replacing with boolean test}} {{6-14=}} {{15-15= != nil}}
  }

  // General case, need to insert parentheses.
  if let x = a ?? a {}  // expected-warning {{value 'x' was defined but never used; consider replacing with boolean test}} {{6-14=(}} {{20-20=) != nil}}
  
  // Special case, we can turn this into an 'is' test.
  if let x = b as? Int {  // expected-warning {{value 'x' was defined but never used; consider replacing with boolean test}} {{6-14=}} {{16-19=is}}
  }

  // SR-14646. Special case, turn this into an 'is' test with optional value. 
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
if let string = optionalAny as? String {} // expected-warning {{value 'string' was defined but never used; consider replacing with boolean test}} {{4-17=}} {{29-32=is}}

// Due to the complexities of global variable tracing, these will not generate warnings
let unusedVariable = ""
var unNeededVar = false
if unNeededVar {}
//guard let foo = optionalAny else { } // FIXME: what do I do?

for i in 0..<10 { // expected-warning {{immutable value 'i' was never used; consider replacing with '_' or removing it}} {{5-6=_}}
   print("")
}

// Tests fix to SR-2421
func sr2421() {
  // FIXME: "consider removing it"?
  let x: Int // expected-warning {{immutable value 'x' was never used; consider replacing with '_' or removing it}}
  x = 42
}

// Tests fix to SR-964
func sr964() {
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

// FIXME: Mark the unused capture
func testLocalFuncNotCalled() {
  var unusedVar = 0
  // expected-warning@-1 {{initialization of variable 'unusedVar' was never used; consider replacing with assignment to '_' or removing it}}

  var notMutatedVar = 0
  // expected-warning@-1 {{initialization of variable 'notMutatedVar' was never used; consider replacing with assignment to '_' or removing it}}

  var mutatedVar = 0
  // expected-warning@-1 {{initialization of variable 'mutatedVar' was never used; consider replacing with assignment to '_' or removing it}}

  func localFunc() {
    _ = notMutatedVar
    mutatedVar = 1
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
  localFunc()
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
//func testVariablesBoundInPatterns() {
//  enum StringB {
//    case simple(b: Bool)
//    case tuple(b: (Bool, Bool))
//    case optional(b: Bool?)
//  }
//
//  // Because Swift enables all kinds of creative binding forms, make sure that
//  // variable patterns occuring directly under a `let` or `var` have that
//  // introducer stripped by the fixit. All other cases are currently too
//  // complicated for the VarDeclUsageChecker.
//  switch StringB.simple(b: true) {
//  case .simple(b: let b) where false: // FIXME: move exxpected-warning {{immutable value 'b' was never used; consider replacing with '_' or removing it}} {{19-24=_}}
//    break
//  case .simple(b: var b) where false: // FIXME: move exxpected-warning {{variable 'b' was never used; consider replacing with '_' or removing it}} {{19-24=_}}
//    break
//  case var .simple(b: b): // FIXME: move exxpected-warning {{variable 'b' was never used; consider replacing with '_' or removing it}} {{23-24=_}}
//    break
//  case .tuple(b: let (b1, b2)) where false:
//    // FIXME: move exxpected-warning@-1 {{immutable value 'b1' was never used; consider replacing with '_' or removing it}} {{23-25=_}}
//    // FIXME: move exxpected-warning@-2 {{immutable value 'b2' was never used; consider replacing with '_' or removing it}} {{27-29=_}}
//    break
//  case .tuple(b: (let b1, let b2)) where false:
//    // FIXME: move exxpected-warning@-1 {{immutable value 'b1' was never used; consider replacing with '_' or removing it}} {{19-25=_}}
//    // FIXME: move exxpected-warning@-2 {{immutable value 'b2' was never used; consider replacing with '_' or removing it}} {{27-33=_}}
//    break
//  case .tuple(b: (var b1, var b2)) where false:
//    // FIXME: move exxpected-warning@-1 {{variable 'b1' was never used; consider replacing with '_' or removing it}} {{19-25=_}}
//    // FIXME: move exxpected-warning@-2 {{variable 'b2' was never used; consider replacing with '_' or removing it}} {{27-33=_}}
//    break
//  case var .tuple(b: (b1, b2)) where false:
//    // FIXME: move exxpected-warning@-1 {{variable 'b1' was never used; consider replacing with '_' or removing it}} {{23-25=_}}
//    // FIXME: move exxpected-warning@-2 {{variable 'b2' was never used; consider replacing with '_' or removing it}} {{27-29=_}}
//    break
//  case .tuple(b: let b): // FIXME: move exxpected-warning {{immutable value 'b' was never used; consider replacing with '_' or removing it}} {{18-23=_}}
//    break
//  case .optional(b: let x?) where false: // FIXME: move exxpected-warning {{immutable value 'x' was never used; consider replacing with '_' or removing it}} {{25-26=_}}
//    break
//  case .optional(b: let .some(x)) where false: // FIXME: move exxpected-warning {{immutable value 'x' was never used; consider replacing with '_' or removing it}} {{31-32=_}}
//    break
//  case let .optional(b: x?): // FIXME: move exxpected-warning {{immutable value 'x' was never used; consider replacing with '_' or removing it}} {{25-26=_}}
//    break
//  case let .optional(b: .none): // FIXME: move exxpected-warning {{'let' pattern has no effect; sub-pattern didn't bind any variables}} {{8-12=}}
//    break
//  }
//}

//===----------------------------------------------------------------------===//
// Top Level Closures
//===----------------------------------------------------------------------===//

var closure_var_unused: () -> Int = {
  var unused = 42 // expected-warning {{initialization of variable 'unused' was never used; consider replacing with assignment to '_' or removing it}}
  return 12
}

var closure_let_unused: () -> Int = {
  let unused = 42 // expected-warning {{initialization of immutable value 'unused' was never used; consider replacing with assignment to '_' or removing it}}
  return 12
}

var closure_var_never_mutated: () -> Int = {
  var unmutated = 42 // expected-warning {{variable 'unmutated' was never mutated; consider changing to 'let' constant}}
  return unmutated
}

func nested_closures() {
  var _: () -> Int = {
    var unused = 42 // expected-warning {{initialization of variable 'unused' was never used; consider replacing with assignment to '_' or removing it}}
    return 12
  }
  
  var _: () -> Int = {
    let unused = 42 // expected-warning {{initialization of immutable value 'unused' was never used; consider replacing with assignment to '_' or removing it}}
    return 12
  }
  
  var _: () -> Int = {
    var unmutated = 42 // expected-warning {{variable 'unmutated' was never mutated; consider changing to 'let' constant}}
    return unmutated
  }
}

//===----------------------------------------------------------------------===//
// Nested Scope Closures
//===----------------------------------------------------------------------===//

class A {
  lazy var lazyvar_var_unused: Int = {
    var unused = 42 // expected-warning {{initialization of variable 'unused' was never used; consider replacing with assignment to '_' or removing it}}
    return 12
  }()
  
  lazy var lazyvar_let_unused: Int = {
    let unused = 42 // expected-warning {{initialization of immutable value 'unused' was never used; consider replacing with assignment to '_' or removing it}}
    return 12
  }()
  
  lazy var lazyvar_var_never_mutated: Int = {
    var unmutated = 42 // expected-warning {{variable 'unmutated' was never mutated; consider changing to 'let' constant}}
    return unmutated
  }()
  
  var closure_var_unused: () -> Int = {
    var unused = 42 // expected-warning {{initialization of variable 'unused' was never used; consider replacing with assignment to '_' or removing it}}
    return 12
  }
  
  var closure_let_unused: () -> Int = {
    let unused = 42 // expected-warning {{initialization of immutable value 'unused' was never used; consider replacing with assignment to '_' or removing it}}
    return 12
  }
  
  var closure_var_never_mutated: () -> Int = {
    var unmutated = 42 // expected-warning {{variable 'unmutated' was never mutated; consider changing to 'let' constant}}
    return unmutated
  }
  
  func nested_closures() {
    var _: () -> Int = {
      var unused = 42 // expected-warning {{initialization of variable 'unused' was never used; consider replacing with assignment to '_' or removing it}}
      return 12
    }
    
    var _: () -> Int = {
      let unused = 42 // expected-warning {{initialization of immutable value 'unused' was never used; consider replacing with assignment to '_' or removing it}}
      return 12
    }
    
    var _: () -> Int = {
      var unmutated = 42 // expected-warning {{variable 'unmutated' was never mutated; consider changing to 'let' constant}}
      return unmutated
    }
  }
}

// Tests fix to SR-14646
func testUselessCastWithInvalidParam(foo: Any?) -> Int {
  class Foo { }
  if let bar = foo as? Foo { return 42 } // expected-warning {{value 'bar' was defined but never used; consider replacing with boolean test}} {{6-16=}} {{20-23=is}}
  else { return 54 }
}
  
//===----------------------------------------------------------------------===//
// FIXME: rename Other SIL Failure Cases
//===----------------------------------------------------------------------===//

struct Y {
  var v: Int
}

class Z {
  let a: Int
  init(y: Y) {
    a = y.v
  }
}

func testPassingLetStructClassReturn() -> Z {
  let y = Y(v: 3)
  let z = Z(y: y)
  return z
}

func testPassingLetStructNoClassReturn() {
  let y = Y(v: 3)
  let z = Z(y: y) // expected-warning {{initialization of immutable value 'z' was never used; consider replacing with assignment to '_' or removing it}}
}

func testPassingVarStructClassReturn() -> Z {
  var y = Y(v: 3) // expected-warning {{variable 'y' was never mutated; consider changing to 'let' constant}}
  let z = Z(y: y)
  return z
}

func testPassingVarStructNoClassReturn() {
  var y = Y(v: 3) // expected-warning {{variable 'y' was never mutated; consider changing to 'let' constant}}
  let z = Z(y: y) // expected-warning {{initialization of immutable value 'z' was never used; consider replacing with assignment to '_' or removing it}}
}

struct IfTester {
  var v: Int
  func doSomething() { }
  func doSomethingElse() { }
}

func testIfDecl(x: IfTester?) {
  let unwrappedX = x!
  if #available(OSX 10.13, iOS 11.0, tvOS 11.0, watchOS 4.0, *) {
    unwrappedX.doSomething()
  } else {
    unwrappedX.doSomethingElse()
  }
}

struct LetPropertyInGetter {
  var values: [Any?] = [1]
  public var property: Int? {
    get {
      let value = values[0]
      return value as? Int
    }
    set {
      values[0] = newValue as Any
    }
  }
}

struct VarPropertyInGetter {
  var values: [Any?] = [1]
  public var property: Int? {
    get {
      var value = values[0] // expected-warning {{variable 'value' was never mutated; consider changing to 'let' constant}}
      return value as? Int
    }
    set {
      values[0] = newValue as Any
    }
  }
}

struct IfLetInGetter {
  struct Nested {
    var string: String
  }
  
  var x: Nested?
  
  public var letRead: String {
    if let u = x {
      return u.string
    } else {
      return ""
    }
  }
  
  public var varRead: String {
    if var u = x { // expected-warning {{variable 'u' was written to, but never read}}
      u.string += "."
      return ""
    } else {
      return ""
    }
  }
  
  public var varReadModified: String {
    if var u = x { // expected-warning {{variable 'u' was never mutated; consider changing to 'let' constant}}
      return u.string
    } else {
      return ""
    }
  }
}

struct ThrowInFunc {
  
  var structError: Error?
  
  public func useThrow() throws -> Bool {
    let error = structError
    if let e = error {
      throw e
    } else {
      return true
    }
  }
}

func substringReturn() -> Substring {
  let s = "hello"
  return s[...]
}


func elementReturn() -> Int {
  let i = [1]
  return i[0]
}

struct AssignInIf {
  
  var a: Bool
  var b: Bool
  
  func test() -> String {
    let result: String
    if (a) {
      result = "a"
    } else if (b) {
      result = "b"
    } else {
      result = "c"
    }
    return result
  }
}

struct MutableThrow {
  mutating func mutate() throws { }
}

func testTryMutate() throws {
  var x = MutableThrow()
  try x.mutate()
}

class GuardPropertyAssign {
  var value: Int
  init?(_ input: Any) {
    guard let integer = input as? Int else {
      return nil
    }
    self.value = integer
  }
}

class Object {
  func foo() { }
}

class GuardClassTest {
  
  var object: Object?
  
  func invalidate() {
    guard let x = self.object else { return }
    x.foo()
  }
}

class ACast { }

struct IfLetEqualityTester {
  
  var x: Any
  
  func unbox(_ value: Any) -> Bool? {
    
    if let number = value as? ACast {
      if number === x as! ACast {
        return true
      }
    }
    return false
  }
}

func testClosureUnread() {
  let body = { () -> Void in // expected-warning {{initialization of immutable value 'body' was never used; consider replacing with assignment to '_' or removing it}}
    print("")
  }
}

func testClosureRead() {
  let body = { () -> Void in
    print("")
  }
  
  body()
}

func testClosureUnreadUnmodified() {
  var body = { () -> Void in // expected-warning {{initialization of variable 'body' was never used; consider replacing with assignment to '_' or removing it}}
    print("")
  }
}

func testClosureUnmodified() {
  var body = { () -> Void in // expected-warning {{variable 'body' was never mutated; consider changing to 'let'}}
    print("")
  }
  body()
}

func testReturnInTuple() -> (Bool, Int) {
  let i = 20
  return (true, i)
}

func testTupleAssignUsed() -> Int {
  let (a, b) = (1, true)
  if b {
    return a
  }
}

func testTupleAssignUsedModified() -> Int {
  var (a, b) = (1, true)
  if b {
    a += 1
    return a
  }
}

func testDeclareInIf() {
  var a = [1,2] // expected-warning {{variable 'a' was written to, but never read}}
  #if arch(arm64)
  let i = 0
  #else
  let i = 1
  #endif
  a[i] = 0
}

enum Switchable {
  case a, b
}

func testSwitch(y: ((Switchable) -> ()) -> ()) -> Int {
  
  do {
    var x = 0
    y { (v) -> () in
      switch v {
      case .a:
        break
      case .b:
        x += 1
      }
    }
    return x
  }
}

struct TestPointerStruct {
  
  typealias Counters = (UInt32, UInt32)
  
  var counters: Counters = (UInt32(0), UInt32(0))
  
  func testPointer(_ index: Int) -> UInt32 {
    var tmpCounters = counters
    
    let counter: UInt32 = withUnsafePointer(to: &tmpCounters) { ptr in
      return ptr.withMemoryRebound(to: UInt32.self, capacity: 64) { buf in
        return buf[index]
      }
    }
    return counter
  }
}

public func testSwitchGeneric<T>(first: (T?, Bool), next: @escaping (T) -> T?) -> T? {
  switch first {
  case (let value, true):
    return value
  case (let value?, _):
    let nextValue = next(value)
    return nextValue
  case (nil, _):
    return nil
  }
}


func trailClosure(c: (Int) -> (Bool, Int)) -> (Bool, Int) {
  return c(5)
}

func TestTupleMutate() -> (Bool, Int) {
  var (a, b): (Bool, Int) = trailClosure { i in
    return (true, i)
  }
  a = false
  return (a, b)
}
