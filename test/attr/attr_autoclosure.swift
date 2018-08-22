// RUN: %target-typecheck-verify-swift

// Simple case.
var fn : @autoclosure () -> Int = 4  // expected-error {{'@autoclosure' may only be used on parameters}}  expected-error {{cannot convert value of type 'Int' to specified type '() -> Int'}}

@autoclosure func func1() {}  // expected-error {{attribute can only be applied to types, not declarations}}

func func1a(_ v1 : @autoclosure Int) {} // expected-error {{@autoclosure attribute only applies to function types}}


func func2(_ fp : @autoclosure () -> Int) { func2(4)}

func func3(fp fpx : @autoclosure () -> Int) {func3(fp: 0)}
func func4(fp : @autoclosure () -> Int) {func4(fp: 0)}
func func6(_: @autoclosure () -> Int) {func6(0)}

// autoclosure + inout doesn't make sense.
func func8(_ x: inout @autoclosure () -> Bool) -> Bool {  // expected-error {{'@autoclosure' may only be used on parameters}}
}

func func9(_ x: @autoclosure (Int) -> Bool) {} // expected-error {{argument type of @autoclosure parameter must be '()'}}
func func10(_ x: @autoclosure (Int, String, Int) -> Void) {} // expected-error {{argument type of @autoclosure parameter must be '()'}}

// <rdar://problem/19707366> QoI: @autoclosure declaration change fixit
let migrate4 : (@autoclosure() -> ()) -> ()


struct SomeStruct {
  @autoclosure let property : () -> Int  // expected-error {{attribute can only be applied to types, not declarations}}

  init() {
  }
}

class BaseClass {
  @autoclosure var property : () -> Int // expected-error {{attribute can only be applied to types, not declarations}}
  init() {}
}

class DerivedClass {
  var property : () -> Int { get {} set {} }
}

protocol P1 {
  associatedtype Element
}
protocol P2 : P1 {
  associatedtype Element
}

func overloadedEach<O: P1>(_ source: O, _ closure: @escaping () -> ()) {
}

func overloadedEach<P: P2>(_ source: P, _ closure: @escaping () -> ()) {
}

struct S : P2 {
  typealias Element = Int
  func each(_ closure: @autoclosure () -> ()) {
    // expected-note@-1{{parameter 'closure' is implicitly non-escaping}}

    overloadedEach(self, closure) // expected-error {{passing non-escaping parameter 'closure' to function expecting an @escaping closure}}
  }
}


struct AutoclosureEscapeTest {
  @autoclosure let delayed: () -> Int  // expected-error {{attribute can only be applied to types, not declarations}}
}

// @autoclosure(escaping)
// expected-error @+1 {{attribute can only be applied to types, not declarations}}
func func10(@autoclosure(escaping _: () -> ()) { } // expected-error{{expected parameter name followed by ':'}}

func func11(_: @autoclosure(escaping) @noescape () -> ()) { } // expected-error{{@escaping conflicts with @noescape}}
  // expected-error@-1{{@autoclosure(escaping) has been removed; use @autoclosure @escaping instead}} {{28-38= @escaping}}

class Super {
  func f1(_ x: @autoclosure(escaping) () -> ()) { }
    // expected-error@-1{{@autoclosure(escaping) has been removed; use @autoclosure @escaping instead}} {{28-38= @escaping}}
  func f2(_ x: @autoclosure(escaping) () -> ()) { } // expected-note {{potential overridden instance method 'f2' here}}
    // expected-error@-1{{@autoclosure(escaping) has been removed; use @autoclosure @escaping instead}} {{28-38= @escaping}}
  func f3(x: @autoclosure () -> ()) { }
}

class Sub : Super {
  override func f1(_ x: @autoclosure(escaping)() -> ()) { }
    // expected-error@-1{{@autoclosure(escaping) has been removed; use @autoclosure @escaping instead}} {{37-47= @escaping }}
  override func f2(_ x: @autoclosure () -> ()) { } // expected-error{{does not override any method}} // expected-note{{type does not match superclass instance method with type '(@autoclosure @escaping () -> ()) -> ()'}}
  override func f3(_ x: @autoclosure(escaping) () -> ()) { }  // expected-error{{does not override any method}}
    // expected-error@-1{{@autoclosure(escaping) has been removed; use @autoclosure @escaping instead}} {{37-47= @escaping}}
}

func func12_sink(_ x: @escaping () -> Int) { }

func func12a(_ x: @autoclosure () -> Int) {
    // expected-note@-1{{parameter 'x' is implicitly non-escaping}}

  func12_sink(x) // expected-error {{passing non-escaping parameter 'x' to function expecting an @escaping closure}}
}
func func12b(_ x: @autoclosure(escaping) () -> Int) {
  // expected-error@-1{{@autoclosure(escaping) has been removed; use @autoclosure @escaping instead}} {{31-41= @escaping}}
  func12_sink(x) // ok
}
func func12c(_ x: @autoclosure @escaping () -> Int) {
  func12_sink(x) // ok
}
func func12d(_ x: @escaping @autoclosure () -> Int) {
  func12_sink(x) // ok
}

class TestFunc12 {
  var x: Int = 5

  func foo() -> Int { return 0 }

  func test() {
    func12a(x + foo()) // okay
    func12b(x + foo()) 
    // expected-error@-1{{reference to property 'x' in closure requires explicit 'self.' to make capture semantics explicit}} {{13-13=self.}}
    // expected-error@-2{{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}} {{17-17=self.}}
  }
}


enum AutoclosureFailableOf<T> {
  case Success(@autoclosure () -> T)
  case Failure
}

let _ : AutoclosureFailableOf<Int> = .Success(42)

let _ : (@autoclosure () -> ()) -> ()
let _ : (@autoclosure(escaping) () -> ()) -> ()
  // expected-error@-1{{@autoclosure(escaping) has been removed; use @autoclosure @escaping instead}} {{22-32= @escaping}}

// escaping is the name of param type
let _ : (@autoclosure(escaping) -> ()) -> ()  // expected-error {{use of undeclared type 'escaping'}}
// expected-error@-1 {{argument type of @autoclosure parameter must be '()'}}

// Migration
// expected-error @+1 {{attribute can only be applied to types, not declarations}}
func migrateAC(@autoclosure _: () -> ()) { }

// expected-error @+1 {{attribute can only be applied to types, not declarations}}
func migrateACE(@autoclosure(escaping) _: () -> ()) { }

func takesAutoclosure(_ fn: @autoclosure () -> Int) {}
func takesThrowingAutoclosure(_: @autoclosure () throws -> Int) {}

func callAutoclosureWithNoEscape(_ fn: () -> Int) {
  takesAutoclosure(1+1) // ok
}
func callAutoclosureWithNoEscape_2(_ fn: () -> Int) {
  takesAutoclosure(fn()) // ok
}
func callAutoclosureWithNoEscape_3(_ fn: @autoclosure () -> Int) {
  takesAutoclosure(fn()) // ok
}

// expected-error @+1 {{'@autoclosure' must not be used on variadic parameters}}
func variadicAutoclosure(_ fn: @autoclosure () -> ()...) {
  for _ in fn {}
}

// rdar://41219750
// These are all arguably invalid; the autoclosure should have to be called.
// But as long as we allow them, we shouldn't crash.
func passNonThrowingToNonThrowingAC(_ fn: @autoclosure () -> Int) {
  takesAutoclosure(fn)
}
func passNonThrowingToThrowingAC(_ fn: @autoclosure () -> Int) {
  takesThrowingAutoclosure(fn)
}
func passThrowingToThrowingAC(_ fn: @autoclosure () throws -> Int) {
  takesThrowingAutoclosure(fn)
}
