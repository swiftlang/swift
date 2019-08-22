// RUN: %target-typecheck-verify-swift

func f0(_ x: Float) -> Float {}
func f1(_ x: Float) -> Float {}
func f2(_ x: @autoclosure () -> Float) {}

var f : Float

_ = f0(f0(f))
_ = f0(1)
_ = f1(f1(f))
f2(f)
f2(1.0)

func call_lvalue(_ rhs: @autoclosure () -> Bool) -> Bool {
  return rhs()
}

// Function returns
func weirdCast<T, U>(_ x: T) -> U {}

func ff() -> (Int) -> (Float) { return weirdCast }

// Block <-> function conversions

var funct: (Int) -> Int = { $0 }
var block: @convention(block) (Int) -> Int = funct
funct = block
block = funct

// Application of implicitly unwrapped optional functions

var optFunc: ((String) -> String)! = { $0 }
var s: String = optFunc("hi")

// <rdar://problem/17652759> Default arguments cause crash with tuple permutation
func testArgumentShuffle(_ first: Int = 7, third: Int = 9) {
}
testArgumentShuffle(third: 1, 2) // expected-error {{unnamed argument #2 must precede argument 'third'}} {{21-21=2, }} {{29-32=}}



func rejectsAssertStringLiteral() {
  assert("foo") // expected-error {{cannot convert value of type 'String' to expected argument type 'Bool'}}
  precondition("foo") // expected-error {{cannot convert value of type 'String' to expected argument type 'Bool'}}
}



// <rdar://problem/22243469> QoI: Poor error message with throws, default arguments, & overloads
func process(_ line: UInt = #line, _ fn: () -> Void) {}
func process(_ line: UInt = #line) -> Int { return 0 }
func dangerous() throws {}

func test() {
  process {         // expected-error {{invalid conversion from throwing function of type '() throws -> Void' to non-throwing function type '() -> Void'}}
    try dangerous()
    test()
  }
}


// <rdar://problem/19962010> QoI: argument label mismatches produce not-great diagnostic
class A {
  func a(_ text:String) {
  }
  func a(_ text:String, something:Int?=nil) {
  }
}
A().a(text:"sometext") // expected-error{{extraneous argument label 'text:' in call}}{{7-12=}}


// <rdar://problem/22451001> QoI: incorrect diagnostic when argument to print has the wrong type
func r22451001() -> AnyObject {}
print(r22451001(5))  // expected-error {{argument passed to call that takes no arguments}}


// SR-590 Passing two parameters to a function that takes one argument of type Any crashes the compiler
// SR-1028: Segmentation Fault: 11 when superclass init takes parameter of type 'Any'
func sr590(_ x: Any) {}  // expected-note {{'sr590' declared here}}
sr590(3,4) // expected-error {{extra argument in call}}
sr590() // expected-error {{missing argument for parameter #1 in call}}
// Make sure calling with structural tuples still works.
sr590(())
sr590((1, 2))

// SR-2657: Poor diagnostics when function arguments should be '@escaping'.
private class SR2657BlockClass<T> { // expected-note 3 {{generic parameters are always considered '@escaping'}}
  let f: T
  init(f: T) { self.f = f }
}

func takesAny(_: Any) {}

func foo(block: () -> (), other: () -> Int) {
  let _ = SR2657BlockClass(f: block)
  // expected-error@-1 {{converting non-escaping value to 'T' may allow it to escape}}
  let _ = SR2657BlockClass<()->()>(f: block)
  // expected-error@-1 {{converting non-escaping parameter 'block' to generic parameter 'T' may allow it to escape}}
  let _: SR2657BlockClass<()->()> = SR2657BlockClass(f: block)
  // expected-error@-1 {{converting non-escaping parameter 'block' to generic parameter 'T' may allow it to escape}}
  let _: SR2657BlockClass<()->()> = SR2657BlockClass<()->()>(f: block)
  // expected-error@-1 {{converting non-escaping parameter 'block' to generic parameter 'T' may allow it to escape}}
  _ = SR2657BlockClass<Any>(f: block)  // expected-error {{converting non-escaping value to 'Any' may allow it to escape}}
  _ = SR2657BlockClass<Any>(f: other) // expected-error {{converting non-escaping value to 'Any' may allow it to escape}}
  takesAny(block)  // expected-error {{converting non-escaping value to 'Any' may allow it to escape}}
  takesAny(other) // expected-error {{converting non-escaping value to 'Any' may allow it to escape}}
}

struct S {
  init<T>(_ x: T, _ y: T) {} // expected-note {{generic parameters are always considered '@escaping'}}
  subscript<T>() -> (T, T) -> Void { { _, _ in } } // expected-note {{generic parameters are always considered '@escaping'}}

  init(fn: () -> Int) {
    self.init({ 0 }, fn) // expected-error {{converting non-escaping parameter 'fn' to generic parameter 'T' may allow it to escape}}
    _ = self[]({ 0 }, fn) // expected-error {{converting non-escaping parameter 'fn' to generic parameter 'T' may allow it to escape}}
  }
}

protocol P {
  associatedtype U
}

func test_passing_noescape_function_to_dependent_member() {
  struct S<T : P> { // expected-note {{generic parameters are always considered '@escaping'}}
    func foo(_: T.U) {}
  }

  struct Q : P {
    typealias U = () -> Int
  }

  func test(_ s: S<Q>, fn: () -> Int) {
    s.foo(fn)
    // expected-error@-1 {{converting non-escaping parameter 'fn' to generic parameter 'T.U' may allow it to escape}}
  }
}

protocol Q {
  associatedtype U : P
}

func sr10811(_ fn: () -> Int) {
  struct S1 : P {
    typealias U = () -> Int
  }

  struct S2 : Q {
    typealias U = S1
  }

  struct S<T : Q> { // expected-note {{generic parameters are always considered '@escaping'}}
    func foo(_ x: T.U.U) {}
  }

  S<S2>().foo(fn) // expected-error {{converting non-escaping parameter 'fn' to generic parameter 'T.U.U' may allow it to escape}}
}

struct Wrapper<U> {
  var value: U
  init(_ value: U) { self.value = value }
}

func with<T>(_ x: T, body: (T) -> Void) {}
func takesGeneric<T>(_ x: T) {}
func takesEscapingFn(_ fn: @escaping () -> Int) {}
func returnsTakesEscapingFn() -> (@escaping () -> Int) -> Void { takesEscapingFn }

prefix operator ^^^
prefix func ^^^(_ x: Int) -> (@escaping () -> Int) -> Void { takesEscapingFn }

func testWeirdFnExprs<T>(_ fn: () -> Int, _ cond: Bool, _ any: Any, genericArg: T) { // expected-note 11{{parameter 'fn' is implicitly non-escaping}}
  (any as! (@escaping () -> Int) -> Void)(fn)
  // expected-error@-1 {{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}

  let wrapped = Wrapper<(@escaping () -> Int) -> Void>({ x in })
  (wrapped[keyPath: \.value] as (@escaping () -> Int) -> Void)(fn)
  // expected-error@-1 {{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}

  (cond ? returnsTakesEscapingFn() : returnsTakesEscapingFn())(fn)
  // expected-error@-1 {{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}

  (^^^5)(fn)
  // expected-error@-1 {{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}

  var optFn: Optional = takesEscapingFn
  optFn?(fn)
  // expected-error@-1 {{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}

  [takesEscapingFn][0](fn)
  // expected-error@-1 {{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}

  (takesEscapingFn, "").0(fn)
  // expected-error@-1 {{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}

  with({ (x: @escaping () -> Int) in }) { y in
    Wrapper(y).value(fn)
    // expected-error @-1{{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}
  }

  _ = { x in (x({ 0 }), x(fn)) }(takesGeneric)
  // expected-error@-1 {{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}

  _ = { (a: (@escaping () -> Int), b) in () }(fn, genericArg)
  // expected-error@-1 {{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}

  func returnsVeryCurried() -> () throws -> (@escaping () -> Int) -> Void { { { x in } } }
  (try? returnsVeryCurried()())?(fn)
  // expected-error@-1 {{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}
}
