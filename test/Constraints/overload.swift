// RUN: %target-typecheck-verify-swift

func markUsed<T>(_ t: T) {}

func f0(_: Float) -> Float {}
// expected-note@-1 {{candidate expects value of type 'Float' for parameter #1 (got 'X')}}
func f0(_: Int) -> Int {}
// expected-note@-1 {{candidate expects value of type 'Int' for parameter #1 (got 'X')}}

func f1(_: Int) {}

func identity<T>(_: T) -> T {}

func f2<T>(_: T) -> T {}
// FIXME: Fun things happen when we make this T, U!
func f2<T>(_: T, _: T) -> (T, T) { }

struct X {}
var x : X
var i : Int
var f : Float

_ = f0(i)
_ = f0(1.0)
_ = f0(1)
f1(f0(1))
f1(identity(1))

f0(x) // expected-error{{no exact matches in call to global function 'f0'}}

_ = f + 1
_ = f2(i)
_ = f2((i, f))

class A { 
  init() {} 
}
class B : A { 
  override init() { super.init() } 
}
class C : B { 
  override init() { super.init() } 
}

func bar(_ b: B) -> Int {} // #1
func bar(_ a: A) -> Float {} // #2

var barResult = bar(C()) // selects #1, which is more specialized
i = barResult // make sure we got #1
f = bar(C()) // selects #2 because of context

// Overload resolution for constructors
protocol P1 { }
struct X1a : P1 { }

struct X1b {
  init(x : X1a) { }
  init<T : P1>(x : T) { }
}

X1b(x: X1a()) // expected-warning{{unused}}

// Overload resolution for subscript operators.
class X2a { }
class X2b : X2a { }
class X2c : X2b { }

struct X2d { 
  subscript (index : X2a) -> Int {
    return 5
  }

  subscript (index : X2b) -> Int {
    return 7
  }

  func foo(_ x : X2c) -> Int {
    return self[x]
  }
}

// Invalid declarations
// FIXME: Suppress the diagnostic for the call below, because the invalid
// declaration would have matched.
func f3(_ x: Intthingy) -> Int { } // expected-error{{cannot find type 'Intthingy' in scope}}

func f3(_ x: Float) -> Float { }
f3(i) // expected-error{{cannot convert value of type 'Int' to expected argument type 'Float'}}

func f4(_ i: Wonka) { } // expected-error{{cannot find type 'Wonka' in scope}}
func f4(_ j: Wibble) { } // expected-error{{cannot find type 'Wibble' in scope}}
f4(5)

func f1() {
  var c : Klass // expected-error{{cannot find type 'Klass' in scope}}
  markUsed(c.x) // make sure error does not cascade here
}

// We don't provide return-type sensitivity unless there is context.
func f5(_ i: Int) -> A { return A() } // expected-note{{candidate}}
func f5(_ i: Int) -> B { return B() } // expected-note{{candidate}}

f5(5) // expected-error{{ambiguous use of 'f5'}}

struct HasX1aProperty {
  func write(_: X1a) {}
  func write(_: P1) {}

  var prop = X1a()
  func test() {
    write(prop) // no error, not ambiguous
  }
}

// rdar://problem/16554496
@available(*, unavailable)
func availTest(_ x: Int) {}
func availTest(_ x: Any) { markUsed("this one") }
func doAvailTest(_ x: Int) {
  availTest(x)
}

// rdar://problem/20886179
func test20886179(_ handlers: [(Int) -> Void], buttonIndex: Int) {
    handlers[buttonIndex](buttonIndex)
}

// The problem here is that the call has a contextual result type incompatible
// with *all* overload set candidates.  This is not an ambiguity.
func overloaded_identity(_ a : Int) -> Int {} // expected-note {{'overloaded_identity' produces 'Int', not the expected contextual result type '()'}} expected-note {{'overloaded_identity' declared her}}
func overloaded_identity(_ b : Float) -> Float {} // expected-note {{'overloaded_identity' produces 'Float', not the expected contextual result type '()'}}

func test_contextual_result_1() {
  return overloaded_identity()  // expected-error {{missing argument for parameter #1 in call}}
  // expected-error@-1 {{no 'overloaded_identity' candidates produce the expected contextual result type '()'}}
}

func test_contextual_result_2() {
  return overloaded_identity(1)
  // expected-error@-1 {{unexpected non-void return value in void function}}
  // expected-note@-2 {{did you mean to add a return type?}}
}

// rdar://problem/24128153
struct X0 {
  init(_ i: Any.Type) { }
  init?(_ i: Any.Type, _ names: String...) { }
}

let x0 = X0(Int.self)
let x0check: X0 = x0 // okay: chooses first initializer

struct X1 {
  init?(_ i: Any.Type) { }
  init(_ i: Any.Type, _ names: String...) { }
}

let x1 = X1(Int.self)
let x1check: X1 = x1 // expected-error{{value of optional type 'X1?' must be unwrapped}}
  // expected-note@-1{{coalesce}}
  // expected-note@-2{{force-unwrap}}


struct X2 {
  init?(_ i: Any.Type) { }
  init(_ i: Any.Type, a: Int = 0) { }
  init(_ i: Any.Type, a: Int = 0, b: Int = 0) { }
  init(_ i: Any.Type, a: Int = 0, c: Int = 0) { }
}

let x2 = X2(Int.self)
let x2check: X2 = x2 // expected-error{{value of optional type 'X2?' must be unwrapped}}
  // expected-note@-1{{coalesce}}
  // expected-note@-2{{force-unwrap}}

// rdar://problem/28051973
struct R_28051973 {
  mutating func f(_ i: Int) {}
  @available(*, deprecated, message: "deprecated")
  func f(_ f: Float) {}
}

let r28051973: Int = 42
R_28051973().f(r28051973) // expected-error {{cannot use mutating member on immutable value: function call returns immutable value}}


// Fix for CSDiag vs CSSolver disagreement on what constitutes a
// valid overload.

func overloadedMethod(n: Int) {}
func overloadedMethod<T>() {} // expected-note {{in call to function 'overloadedMethod()'}}
// expected-error@-1 {{generic parameter 'T' is not used in function signature}}

overloadedMethod()
// expected-error@-1 {{generic parameter 'T' could not be inferred}}

/// https://github.com/apple/swift/issues/46402
/// Ensure we select the overload of `??` returning `T?` rather than `T`.
func f_46402(_ d: [String : Any], _ s: String, _ t: String) -> Any {
  if let r = d[s] ?? d[t] {
    return r
  } else {
    return 0
  }
}

// Overloading with mismatched labels.
func f6<T>(foo: T) { }
func f6<T: P1>(bar: T) { }

struct X6 {
	init<T>(foo: T) { }
	init<T: P1>(bar: T) { }
}

func test_f6() {
	let _: (X1a) -> Void = f6
	let _: (X1a) -> X6 = X6.init
}

func curry<LHS, RHS, R>(_ f: @escaping (LHS, RHS) -> R) -> (LHS) -> (RHS) -> R {
  return { lhs in { rhs in f(lhs, rhs) } }
}

// We need to have an alternative version of this to ensure that there's an overload disjunction created.
func curry<F, S, T, R>(_ f: @escaping (F, S, T) -> R) -> (F) -> (S) -> (T) -> R {
  return { fst in { snd in { thd in f(fst, snd, thd) } } }
}

// Ensure that we consider these unambiguous
let _ = curry(+)(1)
let _ = [0].reduce(0, +)
let _ = curry(+)("string vs. pointer")


func autoclosure1<T>(_: T, _: @autoclosure () -> X) { }

func autoclosure1<T>(_: [T], _: X) { }

func test_autoclosure1(ia: [Int]) {
  autoclosure1(ia, X()) // okay: resolves to the second function
}

// rdar://problem/64368545 - failed to produce diagnostic (hole propagated to func result without recording a fix)
func test_no_hole_propagation() {
  func test(withArguments arguments: [String]) -> String {
    return arguments.reduce(0, +) // expected-error {{cannot convert value of type 'Int' to expected argument type 'String'}}
  }
}

// rdar://79672230 - crash due to unsatisfied `: AnyObject` requirement
func rdar79672230() {
  struct MyType {}

  func test(_ representation: MyType) -> Bool {} // expected-note {{found candidate with type 'MyType'}}
  func test<T>(_ object: inout T) -> Bool where T : AnyObject {} // expected-note {{candidate requires that 'MyType' conform to 'AnyObject' (requirement specified as 'T' : 'AnyObject')}}

  var t: MyType = MyType()
  test(&t) // expected-error {{no exact matches in call to local function 'test'}}
}

// rdar://97396399 - crash in swift::DiagnosticEngine::formatDiagnosticText
func rdar97396399() {
  // Has to be overloaded to make sure that contextual type is not recorded during constraint generation
  func test(_: () -> Void) {}
  func test(_: (Int) -> Void) {}

  // Multiple different overloads, none of which conform to Sequence
  func fn(_: Int) -> Int {}
  // expected-note@-1 {{found candidate with type '(Int) -> Int'}}
  // expected-note@-2 {{result type 'Int' of 'fn' does not conform to 'Sequence'}}
  func fn(_: Int) -> Double {}
  // expected-note@-1 {{found candidate with type '(Int) -> Double'}}
  // expected-note@-2 {{result type 'Double' of 'fn' does not conform to 'Sequence'}}

  test {
    for x in fn { // expected-error {{no 'fn' overloads produce result type that conforms to 'Sequence'}}
      print(x)
    }
  }

  test {
    for x in fn(42) { // expected-error {{no 'fn' overloads produce result type that conforms to 'Sequence'}}
      print(x)
    }
  }
}

// https://github.com/apple/swift/issues/63834
func f63834(int: Int, string: String) {} // expected-note 3{{found candidate with type '(Int, String) -> ()'}}
func f63834(int: Int, string: Bool) {} // expected-note 3{{found candidate with type '(Int, Bool) -> ()'}}

func f63834_1(int: Int, string: Bool) {} // expected-note{{candidate '(Int, Bool) -> ()' has 2 parameters, but context '(Int) -> Void' has 1}}
func f63834_1(int: Int, string: String) {} // expected-note{{candidate '(Int, String) -> ()' has 2 parameters, but context '(Int) -> Void' has 1}}

// FIXME: We can mention candidate type.
func f63834_2(int: Int, string: Bool) {} // expected-note {{found this candidate}}
func f63834_2(int: Int, string: String) {} // expected-note {{found this candidate}}

// One function argument mismatch
let _ = f63834(int:string:) as (Int, Int) -> Void // expected-error{{no exact matches in reference to global function 'f63834'}}
// Contextual mismatch
let _ = f63834(int:string:) as Int // expected-error{{no exact matches in reference to global function 'f63834'}}
let _ = (f63834(int:string:)) as Int // expected-error{{no exact matches in reference to global function 'f63834'}}

// Missing function argument
let _ = f63834_1(int:string:) as (Int) -> Void // expected-error{{no exact matches in reference to global function 'f63834_1'}}
// None of the function argument types matches
let _ = f63834_2(int:string:) as (Double, Double) -> Void // expected-error{{no exact matches in reference to global function 'f63834_2'}}
let _ = { i, j in } as (Int) -> Void // expected-error{{contextual closure type '(Int) -> Void' expects 1 argument, but 2 were used in closure body}}

struct A63834 {
  static func fn(int: Int, string: String) {} // expected-note{{candidate '(Int, String) -> ()' has 2 parameters, but context '(Int) -> Void' has 1}}
  static func fn(int: Int, string: Bool) {}  // expected-note{{candidate '(Int, Bool) -> ()' has 2 parameters, but context '(Int) -> Void' has 1}}

  static func fn1(int: Int, string: String) {} // expected-note{{found candidate with type '(Int, String) -> ()'}}
  static func fn1(int: Int, string: Bool) {}  // expected-note{{found candidate with type '(Int, Bool) -> ()'}}
}
let _ = A63834.fn1(int:string:) as Int // expected-error {{no exact matches in reference to static method 'fn1'}}
let _ = A63834.fn(int:string:) as (Int) -> Void // expected-error{{no exact matches in reference to static method 'fn'}}

typealias Magic<T> = T
func f63834_D(_ x: Int = 0) {}
func f63834_D(_ x: String) {}

(f63834_D as Magic)() // expected-error{{missing argument for parameter #1 in call}}

func fn63834_3() -> String {} // expected-note {{found candidate with type 'String'}}
func fn63834_3() -> Double {} // expected-note {{found candidate with type 'Double'}}

fn63834_3() as Int // expected-error {{no exact matches in call to global function 'fn63834_3'}}

// Make sure that Copyable and/or Escapable don't change overloading behavior
do {
  struct S {
    var v: Int
  }

  func test(data: [S]) {
    let transformed = data.flatMap { e in
      if true {
        return Array<S>()
      }
      return Array(arrayLiteral: e)
    }

    _ = transformed.map {
      _ = $0.v // Ok
    }
  }
}

do {
  struct S {
    let x: Int
    var y: Int
  }

  func overloaded(_: String) {}
  // expected-note@-1 3 {{candidate expects value of type 'String' for parameter #1 (got 'Int')}}
  func overloaded(_: inout Int) {}
  // expected-note@-1 3 {{candidate expects in-out value for parameter #1 but argument is immutable}}

  func testImmutable(s: S) {
    overloaded(s.x) // expected-error {{no exact matches in call to local function 'overloaded'}}
  }
  
  func testMutable(s: inout S) {
    overloaded(s.x) // expected-error {{no exact matches in call to local function 'overloaded'}}
  }

  func testImmutableBase(s: S) {
    overloaded(s.y) // expected-error {{no exact matches in call to local function 'overloaded'}}
  }

  func testMissingAddressOf(s: inout S) {
    overloaded(s.y) // expected-error {{passing value of type 'Int' to an inout parameter requires explicit '&'}}
  }
}
