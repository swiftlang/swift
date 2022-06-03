// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.50 -disable-objc-attr-requires-foundation-module
// RUN: not %target-swift-frontend -target %target-cpu-apple-macosx10.50 -disable-objc-attr-requires-foundation-module -typecheck %s 2>&1 | %FileCheck %s '--implicit-check-not=<unknown>:0'

// Make sure we do not emit availability errors or warnings when -disable-availability-checking is passed
// RUN: not %target-swift-frontend -target %target-cpu-apple-macosx10.50 -typecheck -disable-objc-attr-requires-foundation-module -disable-availability-checking %s 2>&1 | %FileCheck %s '--implicit-check-not=error:'

// REQUIRES: OS=macosx

func hedge() {
  struct Value {}
  
  // We rely on not allowing nesting of extensions, so test to make sure
  // this emits an error.
  // CHECK:error: declaration is only valid at file scope
  extension Value { } // expected-error {{declaration is only valid at file scope}}
}

protocol P<T> {
  associatedtype T
}

struct Wrapper<T> {}

func identity<T>(_ x: any P<T>) -> any P<T> { return x } // OK
func unwrapUnwrap<T>(_ x: (any P<T>)???) -> (any P<T>)? { return x!! } // OK

func erase<T>(_ x: any P<T>) -> Any { return x } // expected-error {{runtime support for parameterized protocol types is only available in}}
// expected-note@-1 {{add @available attribute to enclosing global function}}
// expected-note@-2 {{add 'if #available' version check}}

func eraseOptional<T>(_ x: (any P<T>)?) -> Any { return x }
// expected-note@-1 {{add @available attribute to enclosing global function}}
// expected-error@-2 {{runtime support for parameterized protocol types is only available in}}
// expected-note@-3 {{add 'if #available' version check}}
// expected-warning@-4 {{expression implicitly coerced from '(any P<T>)?' to 'Any'}}
// expected-note@-5 {{provide a default value to avoid this warning}}
// expected-note@-6 {{force-unwrap the value to avoid this warning}}
// expected-note@-7 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}

func eraseOptional2<T>(_ x: (any P<T>)?) -> Any { return x as Any }
// expected-note@-1 {{add @available attribute to enclosing global function}}
// expected-error@-2 {{runtime support for parameterized protocol types is only available in}}
// expected-note@-3 {{add 'if #available' version check}}

func tupleOut<T>() -> (any P<T>, Int) { return tupleOut() } // expected-error {{runtime support for parameterized protocol types is only available in}}
// expected-note@-1 {{add @available attribute to enclosing global function}}
func tupleIn<T>(_ xs: (any P<T>, Int)) -> Int { return tupleIn(xs) } // expected-error {{runtime support for parameterized protocol types is only available in}}
// expected-note@-1 {{add @available attribute to enclosing global function}}
func wrap<T>(_ x: any P<T>) -> Wrapper<any P<T>> { return wrap(x) } // expected-error {{runtime support for parameterized protocol types is only available in}}
// expected-note@-1 {{add @available attribute to enclosing global function}}
func optionalWrap<T>(_ x: any P<T>) -> Wrapper<(any P<T>)?> { return optionalWrap(x) } // expected-error {{runtime support for parameterized protocol types is only available in}}
// expected-note@-1 {{add @available attribute to enclosing global function}}

struct UnavailableWitness: P { // expected-note {{add @available attribute to enclosing struct}}
  typealias T = any P<String> // expected-error {{runtime support for parameterized protocol types is only available in}}
  // expected-note@-1 {{add @available attribute to enclosing type alias}}
}

struct UnavailableOptionalWitness: P { // expected-note {{add @available attribute to enclosing struct}}
  typealias T = (any P<String>)? // expected-error {{runtime support for parameterized protocol types is only available in}}
  // expected-note@-1 {{add @available attribute to enclosing type alias}}
}

struct UnavailableWrappedWitness: P { // expected-note 2 {{add @available attribute to enclosing struct}}
  typealias T = Wrapper<any P<String>> // expected-error 2 {{runtime support for parameterized protocol types is only available in}}
  // expected-note@-1 2 {{add @available attribute to enclosing type alias}}
}

struct ParameterizedMembers { // expected-note {{add @available attribute to enclosing struct}}
  var ok: any P<String>
  var okOptional: (any P<String>)?

  var broken: Wrapper<(any P<String>)?> // expected-error {{runtime support for parameterized protocol types is only available in}}
}

func casts() { // expected-note 5 {{add @available attribute to enclosing global function}}
  struct Value: P { typealias T = String }

  let _ = Value() as any P<String> // OK
  let _ = Value() as! any P<String>
  // expected-warning@-1 {{forced cast from 'Value' to 'any P<String>' always succeeds; did you mean to use 'as'?}}
  // expected-error@-2 {{runtime support for parameterized protocol types is only available in}}
  // expected-note@-3 {{add 'if #available' version check}}

  let _ = Value() is any P<String>
  // expected-warning@-1 {{'is' test is always true}}
  // expected-error@-2 {{runtime support for parameterized protocol types is only available in}}
  // expected-note@-3 {{add 'if #available' version check}}

  let _ = Value() is (any P<String>)???
  // expected-warning@-1 {{'is' test is always true}}
  // expected-error@-2 {{runtime support for parameterized protocol types is only available in}}
  // expected-note@-3 {{add 'if #available' version check}}

  let _ = Value() as! (any P<String>, Int)
  // expected-warning@-1 {{cast from 'Value' to unrelated type '(any P<String>, Int)' always fails}}
  // expected-error@-2 2 {{runtime support for parameterized protocol types is only available in}}
  // expected-note@-3 2 {{add 'if #available' version check}}
}

// FIXME: It's a little aggressive to also ban metatypes.
func metatypes<T>(_ x: T.Type) {  // expected-note 2 {{add @available attribute to enclosing global function}}
  metatypes((any P<T>).self)
  // expected-error@-1 {{runtime support for parameterized protocol types is only available in}}
  // expected-note@-2 {{add 'if #available' version check}}

  metatypes((any P<T>.Type).self)
  // expected-error@-1 {{runtime support for parameterized protocol types is only available in}}
  // expected-note@-2 {{add 'if #available' version check}}
}
