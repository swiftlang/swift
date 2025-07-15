// RUN: %target-swift-frontend %s -emit-sil -verify

// MARK: Relocated Test Cases
// Missing return diagnostics used to also be implemented during parsing/AST
// construction in addition to the SIL passes. Some existing test cases have
// been moved here after removing the earlier phases' diagnostics in favor of
// those implemented via the SIL passes.

// MARK: `decl/subscript/subscripting`

struct MissingGetterSubscript1 {
  subscript (i : Int) -> Int {
  } // expected-error {{missing return in getter for subscript expected to return 'Int'}}
}

// MARK: `decl/var/properties`

struct X {}

var x13: X {} // expected-error {{missing return in getter for var expected to return 'X'}}

struct X14 {}
extension X14 {
  var x14: X {
  } // expected-error {{missing return in getter for property expected to return 'X'}}
}

// https://github.com/apple/swift/issues/57936

enum E1_57936 {
  var foo: Int {} // expected-error{{missing return in getter for property expected to return 'Int'}}
}

enum E2_57936<T> {
  var foo: T {} // expected-error{{missing return in getter for property expected to return 'T'}}
}

// MARK: `decl/var/result_builders`

@resultBuilder
struct Maker {
  static func buildBlock() -> Int { 42 }
}

@Maker
var globalWithEmptyImplicitGetter: Int {}

// MARK: `Parse/omit_return`

var fv_nop: () {
}

var fv_missing: String {
} // expected-error {{missing return in getter for var expected to return 'String'}}

enum S_nop {
    subscript() -> () {
    }
}

enum S_missing {
    subscript() -> String {
    } // expected-error {{missing return in getter for subscript expected to return 'String'}}
}

// MARK: `Sema/generic-subscript`

struct S_generic_subscript_missing_return {
  subscript<Value>(x: Int) -> Value {
  }  // expected-error {{missing return in getter for subscript expected to return 'Value'}}
}

// MARK: New Test Cases

enum MyEmptyType {}
extension MyEmptyType {
  var i: Int {} // expected-error{{missing return in getter for property expected to return 'Int'}}
  var n: MyEmptyType {} // expected-error{{getter for property with uninhabited return type 'MyEmptyType' is missing call to another never-returning function on all paths}}

  static subscript<A>(root: MyEmptyType) -> A {}

  subscript(_ e: MyEmptyType) -> Int {}
  subscript<T>(_ e: MyEmptyType) -> T {}
  subscript(_ i: Int) -> Int {} // expected-error{{missing return in getter for subscript expected to return 'Int'}}
  subscript<T>(_ p: Int) -> T {} // expected-error{{missing return in getter for subscript expected to return 'T'}}
  subscript(_ i: Int) -> Self {} // expected-error{{getter for subscript with uninhabited return type 'MyEmptyType' is missing call to another never-returning function on all paths}}
  subscript(_ s: Self) -> Self {}

  static func unreachable_static_implicit_return(_ e: MyEmptyType) -> Int {}
  func unreachable(_ e: MyEmptyType) -> Int { // expected-note{{'e' is of type 'MyEmptyType' which cannot be constructed because it is an enum with no cases}}
    42 // expected-warning{{will never be executed}}
  }

  // FIXME: should these produce warnings since they implicity take an uninhabited 'self' param?
  func implicitly_unreachable() { _ = 42 }
  func implicitly_unreachable_implicit_return() -> Int { 42 }
}

extension Never {
  var i: Int {} // expected-error{{missing return in getter for property expected to return 'Int'}}
  var n: Never {} // expected-error{{getter for property with uninhabited return type 'Never' is missing call to another never-returning function on all paths}}

  static subscript<A>(root: Never) -> A {}

  subscript(_ n: Never) -> Int {}
  subscript<T>(_ e: Never) -> T {}
  subscript(_ i: Int) -> Int {} // expected-error{{missing return in getter for subscript expected to return 'Int'}}
  subscript<T>(_ p: Int) -> T {} // expected-error{{missing return in getter for subscript expected to return 'T'}}
  subscript(_ i: Int) -> Self {} // expected-error{{getter for subscript with uninhabited return type 'Never' is missing call to another never-returning function on all paths}}
  subscript(_ s: Self) -> Self {}

  static func unreachable_static_implicit_return(_ n: Never) -> Int {}
  func unreachable(_ n: Never) -> Int { // expected-note{{'n' is of type 'Never' which cannot be constructed because it is an enum with no cases}}
    42 // expected-warning{{will never be executed}}
  }

  // FIXME: should these produce unreachable code warnings since they implicity take an uninhabited 'self' param?
  func implicitly_unreachable() { _ = 42 }
  func implicitly_unreachable_implicit_return() -> Int { 42 }
}

enum InhabitedType {
  case inhabitant

  // Uninhabited params
  subscript(_ n: Never) -> Int {}
  subscript<T>(_ e: Never) -> T {}
  subscript(_ v: MyEmptyType, e: Int) -> Never {}

  // Inhabited params
  subscript(_ i: Int) -> Int {} // expected-error{{missing return in getter for subscript expected to return 'Int'}}
  subscript(_ j: Int) -> Void {}
  subscript(_ k: Int) -> Never {} // expected-error{{getter for subscript with uninhabited return type 'Never' is missing call to another never-returning function on all paths}}
  // FIXME: ^ this diagnostic should probably use the word 'subscript' rather than 'getter'
}
