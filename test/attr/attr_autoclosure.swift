// RUN: %target-typecheck-verify-swift -swift-version 5

// Simple case.
var fn : @autoclosure () -> Int = 4  // expected-error {{'@autoclosure' may only be used on parameters}}

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

// @autoclosure(escaping) is no longer a thing; just make sure we don't crash
// expected-error @+1 {{attribute can only be applied to types, not declarations}}
func func10(@autoclosure(escaping _: () -> ()) { } // expected-error{{expected parameter name followed by ':'}}

func func11(_: @autoclosure(escaping) @noescape () -> ()) { } // expected-error{{cannot find type 'escaping' in scope}}
// expected-error @-1 {{attribute can only be applied to types, not declarations}}
// expected-error @-2 {{expected ',' separator}}
// expected-error @-3 {{expected parameter name followed by ':'}}

func func12_sink(_ x: @escaping () -> Int) { }

func func12a(_ x: @autoclosure () -> Int) {
  // expected-note@-1{{parameter 'x' is implicitly non-escaping}}

  func12_sink(x) // expected-error {{passing non-escaping parameter 'x' to function expecting an @escaping closure}}
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
    func12c(x + foo())
    // expected-error@-1{{reference to property 'x' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note@-1{{reference 'self.' explicitly}} {{13-13=self.}}
    // expected-error@-2{{call to method 'foo' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note@-2{{reference 'self.' explicitly}} {{17-17=self.}}
  }
}


enum AutoclosureFailableOf<T> {
  case Success(@autoclosure () -> T)
  case Failure
}

let _ : AutoclosureFailableOf<Int> = .Success(42)

let _ : (@autoclosure () -> ()) -> ()

// escaping is the name of param type
let _ : (@autoclosure(escaping) -> ()) -> ()  // expected-error {{cannot find type 'escaping' in scope}}

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
  takesAutoclosure(fn) // expected-error {{add () to forward @autoclosure parameter}} {{22-22=()}}
}
func passNonThrowingToThrowingAC(_ fn: @autoclosure () -> Int) {
  takesThrowingAutoclosure(fn) // expected-error {{add () to forward @autoclosure parameter}} {{30-30=()}}
}
func passThrowingToThrowingAC(_ fn: @autoclosure () throws -> Int) {
  takesThrowingAutoclosure(fn) // expected-error {{add () to forward @autoclosure parameter}} {{30-30=()}}
}

func passAutoClosureToSubscriptAndMember(_ fn: @autoclosure () -> Int) {
  struct S {
    func bar(_: Int, _ fun: @autoclosure () -> Int) {}

    subscript(_ fn: @autoclosure () -> Int) -> Int { return fn() }

    static func foo(_ fn: @autoclosure () -> Int) {}
  }

  let s = S()
  let _ = s.bar(42, fn) // expected-error {{add () to forward @autoclosure parameter}} {{23-23=()}}
  let _ = s[fn] // expected-error {{add () to forward @autoclosure parameter}} {{15-15=()}}
  let _ = S.foo(fn) // expected-error {{add () to forward @autoclosure parameter}} {{19-19=()}}
}

func passAutoClosureToEnumCase(_ fn: @autoclosure () -> Int) {
  enum E {
    case baz(@autoclosure () -> Int)
  }

  let _: E = .baz(42) // Ok
  let _: E = .baz(fn) // expected-error {{add () to forward @autoclosure parameter}} {{21-21=()}}
}

// rdar://problem/20591571 - Various type inference problems with @autoclosure
func rdar_20591571() {
  func foo(_ g: @autoclosure () -> Int) {
    typealias G = ()->Int
    let _ = unsafeBitCast(g, to: G.self) // expected-error {{converting non-escaping parameter 'g' to generic parameter 'T' may allow it to escape}}
  }

  func id<T>(_: T) -> T {} // expected-note {{eneric parameters are always considered '@escaping'}}
  func same<T>(_: T, _: T) {}
  // expected-note@-1 2 {{generic parameters are always considered '@escaping'}}

  func takesAnAutoclosure(_ fn: @autoclosure () -> Int, _ efn: @escaping @autoclosure () -> Int) {
    // These are OK -- they count as non-escaping uses
    var _ = fn
    let _ = fn

    var _ = efn
    let _ = efn

    _ = id(fn)          // expected-error {{converting non-escaping parameter 'fn' to generic parameter 'T' may allow it to escape}}
    _ = same(fn, { 3 }) // expected-error {{converting non-escaping parameter 'fn' to generic parameter 'T' may allow it to escape}}
    _ = same({ 3 }, fn) // expected-error {{converting non-escaping parameter 'fn' to generic parameter 'T' may allow it to escape}}

    withoutActuallyEscaping(fn) { _ in }              // Ok
    withoutActuallyEscaping(fn) { (_: () -> Int) in } // Ok
  }
}

// rdar://problem/30906031 - [SR-4188]: withoutActuallyEscaping doesn't accept an @autoclosure argument
func rdar_30906031(in arr: [Int], fn: @autoclosure () -> Int) -> Bool {
  return withoutActuallyEscaping(fn) { escapableF in // Ok
    arr.lazy.filter { $0 >= escapableF() }.isEmpty
  }
}

// SR-2688
class Foo {
  typealias FooClosure = () -> String
  func fooFunction(closure: @autoclosure FooClosure) {} // ok
}

class Bar {
  typealias BarClosure = (String) -> String
  func barFunction(closure: @autoclosure BarClosure) {} // expected-error {{argument type of @autoclosure parameter must be '()'}}
}

func rdar_47586626() {
  struct S {}
  typealias F = () -> S

  func foo(_: @autoclosure S) {} // expected-error {{@autoclosure attribute only applies to function types}}
  func bar(_: @autoclosure F) {} // Ok

  let s = S()

  foo(s) // ok
  bar(s) // ok
}

protocol P_47586626 {
  typealias F = () -> Int
  typealias G<T> = () -> T

  func foo(_: @autoclosure F)
  func bar<T>(_: @autoclosure G<T>)
}

func overloaded_autoclj<T>(_: @autoclosure () -> T) {}
func overloaded_autoclj(_: @autoclosure () -> Int) {}

func autoclosure_param_returning_func_type() {
  func foo(_ fn: @autoclosure () -> (() -> Int)) {}
  func generic_foo<T>(_ fn: @autoclosure () -> T) {} // expected-note {{generic parameters are always considered '@escaping'}}

  func bar_1(_ fn: @autoclosure @escaping () -> Int) { foo(fn) } // Ok
  func bar_2(_ fn: @autoclosure () -> Int) { foo(fn) } // expected-note {{parameter 'fn' is implicitly non-escaping}}
  // expected-error@-1 {{passing non-escaping parameter 'fn' to function expecting an @escaping closure}}
  func baz_1(_ fn: @autoclosure @escaping () -> Int) { generic_foo(fn) }   // Ok (T is inferred as () -> Int)
  func baz_2(_ fn: @autoclosure @escaping () -> Int) { generic_foo(fn()) } // Ok (T is inferred as Int)
  func baz_3(_ fn: @autoclosure () -> Int) { generic_foo(fn) } // Fails because fn is not marked as @escaping
  // expected-error@-1 {{converting non-escaping parameter 'fn' to generic parameter 'T' may allow it to escape}}

  // Let's make sure using `fn` as value works fine in presence of overloading
  func biz_1(_ fn: @autoclosure @escaping () -> Int) { overloaded_autoclj(fn) }   // Ok
  func biz_2(_ fn: @autoclosure @escaping () -> Int) { overloaded_autoclj(fn()) } // Ok
  func biz_3(_ fn: @autoclosure () -> Int) { overloaded_autoclj(fn) } // Fails because fn is not marked as @escaping
  // expected-error@-1 {{add () to forward @autoclosure parameter}} {{67-67=()}}

  func fiz(_: @autoclosure () -> (() -> Int)) {}

  func biz_4(_ fn: @autoclosure @escaping () -> (() -> Int)) { fiz(fn) } // Can't forward in Swift >= 5 mode
  // expected-error@-1 {{add () to forward @autoclosure parameter}} {{70-70=()}}
  func biz_5(_ fn: @escaping () -> (() -> Int)) { fiz(fn) } // Can't forward in Swift >= 5 mode
  // expected-error@-1 {{add () to forward @autoclosure parameter}} {{57-57=()}}
}

func test_autoclosure_with_generic_argument_mismatch() {
  struct S<T> {} // expected-note {{arguments to generic parameter 'T' ('String' and 'Int') are expected to be equal}}
  func foo(_: @autoclosure () -> S<Int>) {}

  foo(S<String>()) // expected-error {{cannot convert value of type 'S<String>' to expected argument type 'S<Int>'}}
}

// SR-11934
func sr_11934(_ x: @autoclosure String...) {} // expected-error {{'@autoclosure' must not be used on variadic parameters}}

// SR-11938
let sr_11938_1: Array<@autoclosure String> = [] // expected-error {{'@autoclosure' may only be used on parameters}}
func sr_11938_2() -> @autoclosure String { "" } // expected-error {{'@autoclosure' may only be used on parameters}}
func sr_11938_3(_ x: [@autoclosure String]) {} // expected-error {{'@autoclosure' may only be used on parameters}}

protocol SR_11938_P {}
struct SR_11938_S : @autoclosure SR_11938_P {} // expected-error {{'@autoclosure' may only be used on parameters}}

// SR-9178
func bar<T>(_ x: @autoclosure T) {} // expected-error 1{{@autoclosure attribute only applies to function types}}

func test_autoclosure_type_in_parens() {
  let _: (@autoclosure (() -> Void)) -> Void = { _ in } // Ok

  struct Test {
    func bugSingle<T: RawRepresentable>(defaultValue: @autoclosure (() -> T)) -> T { // Ok
      defaultValue()
    }

    func bugMultiple<T: RawRepresentable>(defaultValue: @autoclosure ((() -> T))) -> T { // Ok
      defaultValue()
    }
  }

  enum E : String {
    case foo = "foo"
    case bar = "bar"
  }

  _ = Test().bugSingle(defaultValue: E.foo)   // Ok
  _ = Test().bugMultiple(defaultValue: E.bar) // Ok
}

func test_autoclosure_with_typealias() {
  typealias ConcreteFunc = () -> Int
  typealias GenericFunc<T> = () -> T

  func test(cr: @autoclosure ConcreteFunc) -> Int { cr() } // Ok
  func test<Q>(gn: @autoclosure GenericFunc<Q>) -> Q { gn() } // Ok

  _ = test(cr: 0) // Ok
  _ = test(gn: 1) // Ok
}
