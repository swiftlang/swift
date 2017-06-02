// RUN: rm -rf %t && mkdir -p %t
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -typecheck -verify -primary-file %t/main.swift %S/Inputs/struct_equatable_hashable_other.swift -verify-ignore-unknown

struct Point: Hashable {
  let x: Int
  let y: Int
}

if Point(x: 1, y: 2) == Point(x: 2, y: 1) { }
var pointHash: Int = Point(x: 3, y: 5).hashValue

struct Pair<T: Hashable>: Hashable {
  let first: T
  let second: T

  func same() -> Bool {
    return first == second
  }
}

let p1 = Pair(first: "a", second: "b")
let p2 = Pair(first: "a", second: "c")
if p1 == p2 { }
var pairHash: Int = p1.hashValue

func localStruct() -> Bool {
  struct Local: Equatable {
    let v: Int
  }

  return Local(v: 5) == Local(v: 4)
}

struct CustomHashable: Hashable {
  let x: Int
  let y: Int

  var hashValue: Int { return 0 }

  static func ==(x: CustomHashable, y: CustomHashable) -> Bool { return true } // expected-note 2 {{non-matching type}}
}

if CustomHashable(x: 1, y: 2) == CustomHashable(x: 2, y: 3) { }
var custHash: Int = CustomHashable(x: 1, y: 2).hashValue

// Check use of an struct's synthesized members before the struct is actually declared.
struct UseStructBeforeDeclaration {
  let eqValue = StructToUseBeforeDeclaration(v: 4) == StructToUseBeforeDeclaration(v: 5)
  let hashValue = StructToUseBeforeDeclaration(v: 1).hashValue
}
struct StructToUseBeforeDeclaration: Hashable {
  let v: Int
}

// Check structs from another file in the same module.
if FromOtherFile(v: "a") == FromOtherFile(v: "b") {}
let _: Int = FromOtherFile(v: "c").hashValue

func getFromOtherFile() -> AlsoFromOtherFile { return AlsoFromOtherFile(v: 4) }
if AlsoFromOtherFile(v: 3) == getFromOtherFile() {}

func overloadFromOtherFile() -> YetAnotherFromOtherFile { return YetAnotherFromOtherFile(v: 1.2) }
func overloadFromOtherFile() -> Bool { return false }
if YetAnotherFromOtherFile(v: 1.9) == overloadFromOtherFile() {}


// Even if the struct has only equatable/hashable members, it's not synthesized
// implicitly.
struct StructWithoutExplicitConformance {
  let a: Int
  let b: String
}

if StructWithoutExplicitConformance(a: 1, b: "b") == StructWithoutExplicitConformance(a: 2, b: "a") { } // expected-error{{binary operator '==' cannot be applied to two 'StructWithoutExplicitConformance' operands}}
// expected-note @-1 {{overloads for '==' exist with these partially matching parameter lists: }}


// Structs with non-hashable/equatable stored properties don't derive conformance.
struct NotHashable {}
struct StructWithNonHashablePayload: Hashable { // expected-error 2 {{does not conform}}
  let a: NotHashable
}

// ...but computed properties and static properties are not considered.
struct StructIgnoresComputedProperties: Hashable {
  var a: Int
  var b: String
  static var staticComputed = NotHashable()
  var computed: NotHashable { return NotHashable() }
}
if StructIgnoresComputedProperties(a: 1, b: "a") == StructIgnoresComputedProperties(a: 2, b: "c") {}
let _: Int = StructIgnoresComputedProperties(a: 3, b: "p").hashValue


// Structs should be able to derive conformances based on the conformances of
// their generic arguments.
struct GenericHashable<T: Hashable>: Hashable {
  let value: T
}
if GenericHashable<String>(value: "a") == GenericHashable<String>(value: "b") { }
var genericHashableHash: Int = GenericHashable<String>(value: "a").hashValue

// But it should be an error if the generic argument doesn't have the necessary
// constrants to satisfy the conditions for derivation.
struct GenericNotHashable<T: Equatable>: Hashable { // expected-error {{does not conform}}
  let value: T
}
if GenericNotHashable<String>(value: "a") == GenericNotHashable<String>(value: "b") { }
var genericNotHashableHash: Int = GenericNotHashable<String>(value: "a").hashValue // expected-error {{value of type 'GenericNotHashable<String>' has no member 'hashValue'}}


// Conformance cannot be synthesized in an extension.
struct StructConformsInExtension {
  let v: Int
}
extension StructConformsInExtension : Equatable {} // expected-error {{cannot be automatically synthesized in an extension yet}}

// But explicit conformance in an extension should work.
public struct StructConformsAndImplementsInExtension {
  let v: Int
}
extension StructConformsAndImplementsInExtension : Equatable {
  public static func ==(lhs: StructConformsAndImplementsInExtension, rhs: StructConformsAndImplementsInExtension) -> Bool {  // expected-note {{non-matching type}}
    return true
  }  
}

// No explicit conformance and it cannot be derived.
struct NotExplicitlyHashableAndCannotDerive {
  let v: NotHashable
}
extension NotExplicitlyHashableAndCannotDerive : Hashable {}  // expected-error 2 {{does not conform}}

// A struct with no stored properties trivially derives conformance.
struct NoStoredProperties: Hashable {}

// Verify that conformance (albeit manually implemented) can still be added to
// a type in a different file.
extension OtherFileNonconforming: Hashable {
  static func ==(lhs: OtherFileNonconforming, rhs: OtherFileNonconforming) -> Bool { // expected-note {{non-matching type}}
    return true
  }
  var hashValue: Int { return 0 }
}
// ...but synthesis in a type defined in another file doesn't work yet.
extension YetOtherFileNonconforming: Equatable {} // expected-error {{cannot be automatically synthesized in an extension yet}}

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected error produced: invalid redeclaration of 'hashValue'
// <unknown>:0: error: unexpected note produced: candidate has non-matching type '(Foo, Foo) -> Bool'
// <unknown>:0: error: unexpected note produced: candidate has non-matching type '<T> (Generic<T>, Generic<T>) -> Bool'
// <unknown>:0: error: unexpected note produced: candidate has non-matching type '(InvalidCustomHashable, InvalidCustomHashable) -> Bool'
// <unknown>:0: error: unexpected note produced: candidate has non-matching type '(EnumToUseBeforeDeclaration, EnumToUseBeforeDeclaration) -> Bool'
