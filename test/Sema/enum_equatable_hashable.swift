// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -typecheck -verify -primary-file %t/main.swift %S/Inputs/enum_equatable_hashable_other.swift -verify-ignore-unknown

enum Foo {
  case A, B
}

if Foo.A == .B { }
var aHash: Int = Foo.A.hashValue

enum Generic<T> {
  case A, B

  static func method() -> Int {
    // Test synthesis of == without any member lookup being done
    if A == B { }
    return Generic.A.hashValue
  }
}

if Generic<Foo>.A == .B { }
var gaHash: Int = Generic<Foo>.A.hashValue

func localEnum() -> Bool {
  enum Local {
    case A, B
  }

  return Local.A == .B
}

enum CustomHashable {
  case A, B

  var hashValue: Int { return 0 }
}
func ==(x: CustomHashable, y: CustomHashable) -> Bool { // expected-note 3 {{non-matching type}}
  return true
}

if CustomHashable.A == .B { }
var custHash: Int = CustomHashable.A.hashValue

// We still synthesize conforming overloads of '==' and 'hashValue' if
// explicit definitions don't satisfy the protocol requirements. Probably
// not what we actually want.
enum InvalidCustomHashable {
  case A, B

  var hashValue: String { return "" } // expected-note{{previously declared here}}
}
func ==(x: InvalidCustomHashable, y: InvalidCustomHashable) -> String { // expected-note 3 {{non-matching type}}
  return ""
}
if InvalidCustomHashable.A == .B { }
var s: String = InvalidCustomHashable.A == .B
s = InvalidCustomHashable.A.hashValue
var i: Int = InvalidCustomHashable.A.hashValue

// Check use of an enum's synthesized members before the enum is actually declared.
struct UseEnumBeforeDeclaration {
  let eqValue = EnumToUseBeforeDeclaration.A == .A
  let hashValue = EnumToUseBeforeDeclaration.A.hashValue
}
enum EnumToUseBeforeDeclaration {
  case A
}

// Check enums from another file in the same module.
if FromOtherFile.A == .A {}
let _: Int = FromOtherFile.A.hashValue

func getFromOtherFile() -> AlsoFromOtherFile { return .A }
if .A == getFromOtherFile() {}

func overloadFromOtherFile() -> YetAnotherFromOtherFile { return .A }
func overloadFromOtherFile() -> Bool { return false }
if .A == overloadFromOtherFile() {}


// Complex enums are not implicitly Equatable or Hashable.
enum Complex {
  case A(Int)
  case B
}

if Complex.A(1) == .B { } // expected-error{{binary operator '==' cannot be applied to operands of type 'Complex' and '_'}}
// expected-note @-1 {{overloads for '==' exist with these partially matching parameter lists: }}

// Enums with equatable payloads are equatable if they explicitly conform.
enum EnumWithEquatablePayload: Equatable {
  case A(Int)
  case B(String, Int)
  case C
}

if EnumWithEquatablePayload.A(1) == .B("x", 1) { }
if EnumWithEquatablePayload.A(1) == .C { }
if EnumWithEquatablePayload.B("x", 1) == .C { }

// Enums with hashable payloads are hashable if they explicitly conform.
enum EnumWithHashablePayload: Hashable {
  case A(Int)
  case B(String, Int)
  case C
}

_ = EnumWithHashablePayload.A(1).hashValue
_ = EnumWithHashablePayload.B("x", 1).hashValue
_ = EnumWithHashablePayload.C.hashValue

// ...and they should also inherit equatability from Hashable.
if EnumWithHashablePayload.A(1) == .B("x", 1) { }
if EnumWithHashablePayload.A(1) == .C { }
if EnumWithHashablePayload.B("x", 1) == .C { }

// Enums with non-hashable payloads don't derive conformance.
struct NotHashable {}
enum EnumWithNonHashablePayload: Hashable { // expected-error 2 {{does not conform}}
  case A(NotHashable)
}

// Enums should be able to derive conformances based on the conformances of
// their generic arguments.
enum GenericHashable<T: Hashable>: Hashable {
  case A(T)
  case B
}
if GenericHashable<String>.A("a") == .B { }
var genericHashableHash: Int = GenericHashable<String>.A("a").hashValue

// But it should be an error if the generic argument doesn't have the necessary
// constrants to satisfy the conditions for derivation.
enum GenericNotHashable<T: Equatable>: Hashable { // expected-error {{does not conform}}
  case A(T)
  case B
}
if GenericNotHashable<String>.A("a") == .B { }
var genericNotHashableHash: Int = GenericNotHashable<String>.A("a").hashValue // expected-error {{value of type 'GenericNotHashable<String>' has no member 'hashValue'}}

// An enum with no cases should not derive conformance.
enum NoCases: Hashable {} // expected-error 2 {{does not conform}}

// rdar://19773050
private enum Bar<T> {
  case E(Unknown<T>)  // expected-error {{use of undeclared type 'Unknown'}}

  mutating func value() -> T {
    switch self {
    // FIXME: Should diagnose here that '.' needs to be inserted, but E has an ErrorType at this point
    case E(let x):
      return x.value
    }
  }
}

// Equatable extension -- rdar://20981254
enum Instrument {
  case Piano
  case Violin
  case Guitar
}

extension Instrument : Equatable {}

// Explicit conformance should work too
public enum Medicine {
  case Antibiotic
  case Antihistamine
}

extension Medicine : Equatable {}

public func ==(lhs: Medicine, rhs: Medicine) -> Bool { // expected-note 2 {{non-matching type}}
  return true
}

// No explicit conformance; it could be derived, but we don't support extensions
// yet.
extension Complex : Hashable {}  // expected-error 2 {{cannot be automatically synthesized in an extension yet}}

// No explicit conformance and it cannot be derived.
enum NotExplicitlyHashableAndCannotDerive {
  case A(NotHashable)
}
extension NotExplicitlyHashableAndCannotDerive : Hashable {} // expected-error 2 {{does not conform}}

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected error produced: invalid redeclaration of 'hashValue'
// <unknown>:0: error: unexpected note produced: candidate has non-matching type '(Foo, Foo) -> Bool'
// <unknown>:0: error: unexpected note produced: candidate has non-matching type '<T> (Generic<T>, Generic<T>) -> Bool'
// <unknown>:0: error: unexpected note produced: candidate has non-matching type '(InvalidCustomHashable, InvalidCustomHashable) -> Bool'
// <unknown>:0: error: unexpected note produced: candidate has non-matching type '(EnumToUseBeforeDeclaration, EnumToUseBeforeDeclaration) -> Bool'
