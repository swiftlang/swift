// RUN: %target-parse-verify-swift

func markUsed<T>(t: T) {}

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

f0(i)
f0(1.0)
f0(1)
f1(f0(1))
f1(identity(1))

f0(x) // expected-error{{cannot find an overload for 'f0' that accepts an argument list of type '(X)'}}

f + 1
f2(i)
f2((i, f))

class A { 
  init() {} 
}
class B : A { 
  override init() { super.init() } 
}
class C : B { 
  override init() { super.init() } 
}

func bar(b: B) -> Int {} // #1
func bar(a: A) -> Float {} // #2

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

X1b(x: X1a())

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

  func foo(x : X2c) -> Int {
    return self[x]
  }
}

// Invalid declarations
// FIXME: Suppress the diagnostic for the call below, because the invalid
// declaration would have matched.
func f3(x: Intthingy) -> Int { } // expected-error{{use of undeclared type 'Intthingy'}}
func f3(x: Float) -> Float { }
f3(i) // expected-error{{cannot find an overload for 'f3' that accepts an argument list of type '(Int)'}}

func f4(i: Wonka) { } // expected-error{{use of undeclared type 'Wonka'}}
func f4(j: Wibble) { } // expected-error{{use of undeclared type 'Wibble'}}
f4(5)

func f1() {
  var c : Class // expected-error{{use of undeclared type 'Class'}}
  markUsed(c.x) // make sure error does not cascade here
}

// We don't provide return-type sensitivity unless there is context.
func f5(i: Int) -> A { return A() } // expected-note{{candidate}}
func f5(i: Int) -> B { return B() } // expected-note{{candidate}}

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
func availTest(x: Int) {}
func availTest(x: Any) { markUsed("this one") }
func doAvailTest(x: Int) {
  availTest(x)
}
