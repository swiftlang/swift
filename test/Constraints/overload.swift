// RUN: %target-typecheck-verify-swift

func markUsed<T>(_ t: T) {}

func f0(_: Float) -> Float {}
func f0(_: Int) -> Int {}

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

f0(x) // expected-error{{cannot invoke 'f0' with an argument list of type '(X)'}}
// expected-note @-1 {{overloads for 'f0' exist with these partially matching parameter lists: (Float), (Int)}}

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
func f3(_ x: Intthingy) -> Int { } // expected-error{{use of undeclared type 'Intthingy'}}

func f3(_ x: Float) -> Float { }
f3(i) // expected-error{{cannot convert value of type 'Int' to expected argument type 'Float'}}

func f4(_ i: Wonka) { } // expected-error{{use of undeclared type 'Wonka'}}
func f4(_ j: Wibble) { } // expected-error{{use of undeclared type 'Wibble'}}
f4(5)

func f1() {
  var c : Class // expected-error{{use of undeclared type 'Class'}}
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
func overloaded_identity(_ a : Int) -> Int {}
func overloaded_identity(_ b : Float) -> Float {}

func test_contextual_result_1() {
  return overloaded_identity()  // expected-error {{cannot invoke 'overloaded_identity' with no arguments}}
  // expected-note @-1 {{overloads for 'overloaded_identity' exist with these partially matching parameter lists: (Int), (Float)}}
}

func test_contextual_result_2() {
  return overloaded_identity(1)  // expected-error {{unexpected non-void return value in void function}}
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
let x1check: X1 = x1 // expected-error{{value of optional type 'X1?' not unwrapped; did you mean to use '!' or '?'?}}


struct X2 {
  init?(_ i: Any.Type) { }
  init(_ i: Any.Type, a: Int = 0) { }
  init(_ i: Any.Type, a: Int = 0, b: Int = 0) { }
  init(_ i: Any.Type, a: Int = 0, c: Int = 0) { }
}

let x2 = X2(Int.self)
let x2check: X2 = x2 // expected-error{{value of optional type 'X2?' not unwrapped; did you mean to use '!' or '?'?}}

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

func overloadedMethod(n: Int) {} // expected-note {{'overloadedMethod(n:)' declared here}}
func overloadedMethod<T>() {}
// expected-error@-1 {{generic parameter 'T' is not used in function signature}}

overloadedMethod()
// expected-error@-1 {{missing argument for parameter 'n' in call}}

// Ensure we select the overload of '??' returning T? rather than T.
func SR3817(_ d: [String : Any], _ s: String, _ t: String) -> Any {
  if let r = d[s] ?? d[t] {
    return r
  } else {
    return 0
  }
}
