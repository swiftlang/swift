// RUN: %target-swift-frontend -typecheck -verify -primary-file %s %S/Inputs/struct_equatable_hashable_other.swift -verify-ignore-unknown -swift-version 4

var hasher = Hasher()

struct Point: Hashable {
  let x: Int
  let y: Int
}

func point() {
  if Point(x: 1, y: 2) == Point(x: 2, y: 1) { }
  let _: Int = Point(x: 3, y: 5).hashValue
  Point(x: 3, y: 5).hash(into: &hasher)

  Point(x: 1, y: 2) == Point(x: 2, y: 1) // expected-warning {{result of operator '==' is unused}}
}

struct Pair<T: Hashable>: Hashable {
  let first: T
  let second: T

  func same() -> Bool {
    return first == second
  }
}

func pair() {
  let p1 = Pair(first: "a", second: "b")
  let p2 = Pair(first: "a", second: "c")
  if p1 == p2 { }
  let _: Int = p1.hashValue
  p1.hash(into: &hasher)
}

func localStruct() -> Bool {
  struct Local: Equatable {
    let v: Int
  }

  return Local(v: 5) == Local(v: 4)
}


//------------------------------------------------------------------------------
// Verify compiler can derive hash(into:) implementation from hashValue

struct CustomHashValue: Hashable {
  let x: Int
  let y: Int

  static func ==(x: CustomHashValue, y: CustomHashValue) -> Bool { return true }
  func hash(into hasher: inout Hasher) {}
}

func customHashValue() {
  if CustomHashValue(x: 1, y: 2) == CustomHashValue(x: 2, y: 3) { }
  let _: Int = CustomHashValue(x: 1, y: 2).hashValue
  CustomHashValue(x: 1, y: 2).hash(into: &hasher)
}


//------------------------------------------------------------------------------
// Verify compiler can derive hashValue implementation from hash(into:)

struct CustomHashInto: Hashable {
  let x: Int
  let y: Int

  func hash(into hasher: inout Hasher) {
    hasher.combine(x)
    hasher.combine(y)
  }

  static func ==(x: CustomHashInto, y: CustomHashInto) -> Bool { return true }
}

func customHashInto() {
  if CustomHashInto(x: 1, y: 2) == CustomHashInto(x: 2, y: 3) { }
  let _: Int = CustomHashInto(x: 1, y: 2).hashValue
  CustomHashInto(x: 1, y: 2).hash(into: &hasher)
}

// Check use of an struct's synthesized members before the struct is actually declared.
struct UseStructBeforeDeclaration {
  let eqValue = StructToUseBeforeDeclaration(v: 4) == StructToUseBeforeDeclaration(v: 5)
  let hashValue = StructToUseBeforeDeclaration(v: 1).hashValue
  let hashInto: (inout Hasher) -> Void = StructToUseBeforeDeclaration(v: 1).hash(into:)
}
struct StructToUseBeforeDeclaration: Hashable {
  let v: Int
}

func getFromOtherFile() -> AlsoFromOtherFile { return AlsoFromOtherFile(v: 4) }
func overloadFromOtherFile() -> YetAnotherFromOtherFile { return YetAnotherFromOtherFile(v: 1.2) }
func overloadFromOtherFile() -> Bool { return false }

func useStructBeforeDeclaration() {
  // Check structs from another file in the same module.
  if FromOtherFile(v: "a") == FromOtherFile(v: "b") {}
  let _: Int = FromOtherFile(v: "c").hashValue
  FromOtherFile(v: "d").hash(into: &hasher)

  if AlsoFromOtherFile(v: 3) == getFromOtherFile() {}

  if YetAnotherFromOtherFile(v: 1.9) == overloadFromOtherFile() {}
}


// Even if the struct has only equatable/hashable members, it's not synthesized
// implicitly.
struct StructWithoutExplicitConformance {
  let a: Int
  let b: String
}

func structWithoutExplicitConformance() {
  if StructWithoutExplicitConformance(a: 1, b: "b") == StructWithoutExplicitConformance(a: 2, b: "a") { } // expected-error{{binary operator '==' cannot be applied to two 'StructWithoutExplicitConformance' operands}}
}

// Structs with non-hashable/equatable stored properties don't derive conformance.
struct NotHashable {}
struct StructWithNonHashablePayload: Hashable { // expected-error 2 {{does not conform}}
  let a: NotHashable // expected-note {{stored property type 'NotHashable' does not conform to protocol 'Hashable', preventing synthesized conformance of 'StructWithNonHashablePayload' to 'Hashable'}}
  // expected-note@-1 {{stored property type 'NotHashable' does not conform to protocol 'Equatable', preventing synthesized conformance of 'StructWithNonHashablePayload' to 'Equatable'}}
}

// ...but computed properties and static properties are not considered.
struct StructIgnoresComputedProperties: Hashable {
  var a: Int
  var b: String
  static var staticComputed = NotHashable()
  var computed: NotHashable { return NotHashable() }
}
func structIgnoresComputedProperties() {
  if StructIgnoresComputedProperties(a: 1, b: "a") == StructIgnoresComputedProperties(a: 2, b: "c") {}
  let _: Int = StructIgnoresComputedProperties(a: 3, b: "p").hashValue
  StructIgnoresComputedProperties(a: 4, b: "q").hash(into: &hasher)
}

// Structs should be able to derive conformances based on the conformances of
// their generic arguments.
struct GenericHashable<T: Hashable>: Hashable {
  let value: T
}
func genericHashable() {
  if GenericHashable<String>(value: "a") == GenericHashable<String>(value: "b") { }
  let _: Int = GenericHashable<String>(value: "c").hashValue
  GenericHashable<String>(value: "c").hash(into: &hasher)
}

// But it should be an error if the generic argument doesn't have the necessary
// constraints to satisfy the conditions for derivation.
struct GenericNotHashable<T: Equatable>: Hashable { // expected-error 2 {{does not conform to protocol 'Hashable'}}
  let value: T // expected-note 2 {{stored property type 'T' does not conform to protocol 'Hashable', preventing synthesized conformance of 'GenericNotHashable<T>' to 'Hashable'}}
}
func genericNotHashable() {
  if GenericNotHashable<String>(value: "a") == GenericNotHashable<String>(value: "b") { }
  let gnh = GenericNotHashable<String>(value: "b")
  let _: Int = gnh.hashValue // No error. hashValue is always synthesized, even if Hashable derivation fails
  gnh.hash(into: &hasher) // expected-error {{value of type 'GenericNotHashable<String>' has no member 'hash'}}
}

// Synthesis can be from an extension...
struct StructConformsInExtension {
  let v: Int
}
extension StructConformsInExtension : Equatable {}

// and explicit conformance in an extension should also work.
public struct StructConformsAndImplementsInExtension {
  let v: Int
}
extension StructConformsAndImplementsInExtension : Equatable {
  public static func ==(lhs: StructConformsAndImplementsInExtension, rhs: StructConformsAndImplementsInExtension) -> Bool {
    return true
  }
}

// No explicit conformance and it cannot be derived.
struct NotExplicitlyHashableAndCannotDerive {
  let v: NotHashable // expected-note {{stored property type 'NotHashable' does not conform to protocol 'Hashable', preventing synthesized conformance of 'NotExplicitlyHashableAndCannotDerive' to 'Hashable'}}
  // expected-note@-1 {{stored property type 'NotHashable' does not conform to protocol 'Equatable', preventing synthesized conformance of 'NotExplicitlyHashableAndCannotDerive' to 'Equatable'}}
}
extension NotExplicitlyHashableAndCannotDerive : Hashable {}  // expected-error 2 {{does not conform}}

// A struct with no stored properties trivially derives conformance.
struct NoStoredProperties: Hashable {}

// Verify that conformance (albeit manually implemented) can still be added to
// a type in a different file.
extension OtherFileNonconforming: Hashable {
  static func ==(lhs: OtherFileNonconforming, rhs: OtherFileNonconforming) -> Bool {
    return true
  }
  func hash(into hasher: inout Hasher) {}
}
// ...but synthesis in a type defined in another file doesn't work yet.
extension YetOtherFileNonconforming: Equatable {} // expected-error {{cannot be automatically synthesized in an extension in a different file to the type}}

// Verify that we can add Hashable conformance in an extension by only
// implementing hash(into:)
struct StructConformsAndImplementsHashIntoInExtension: Equatable {
  let v: String
}
extension StructConformsAndImplementsHashIntoInExtension: Hashable {
  func hash(into hasher: inout Hasher) {
    hasher.combine(v)
  }
}
func structConformsAndImplementsHashIntoInExtension() {
  let _: Int = StructConformsAndImplementsHashIntoInExtension(v: "a").hashValue
  StructConformsAndImplementsHashIntoInExtension(v: "b").hash(into: &hasher)
}

struct GenericHashIntoInExtension<T: Hashable>: Equatable {
  let value: T
}
extension GenericHashIntoInExtension: Hashable {
  func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }
}
func genericHashIntoInExtension() {
  let _: Int = GenericHashIntoInExtension<String>(value: "a").hashValue
  GenericHashIntoInExtension(value: "b").hash(into: &hasher)
}

// Conditional conformances should be able to be synthesized
struct GenericDeriveExtension<T> {
    let value: T
}
extension GenericDeriveExtension: Equatable where T: Equatable {}
extension GenericDeriveExtension: Hashable where T: Hashable {}

// Incorrectly/insufficiently conditional shouldn't work
struct BadGenericDeriveExtension<T> {
    let value: T // expected-note {{stored property type 'T' does not conform to protocol 'Hashable', preventing synthesized conformance of 'BadGenericDeriveExtension<T>' to 'Hashable'}}
// expected-note@-1 {{stored property type 'T' does not conform to protocol 'Equatable', preventing synthesized conformance of 'BadGenericDeriveExtension<T>' to 'Equatable'}}
}
extension BadGenericDeriveExtension: Equatable {}
// expected-error@-1 {{type 'BadGenericDeriveExtension<T>' does not conform to protocol 'Equatable'}}
extension BadGenericDeriveExtension: Hashable where T: Equatable {}
// expected-error@-1 {{type 'BadGenericDeriveExtension' does not conform to protocol 'Hashable'}}

// But some cases don't need to be conditional, even if they look similar to the
// above
struct AlwaysHashable<T>: Hashable {}
struct UnusedGenericDeriveExtension<T> {
    let value: AlwaysHashable<T>
}
extension UnusedGenericDeriveExtension: Hashable {}

// Cross-file synthesis is still disallowed for conditional cases
extension GenericOtherFileNonconforming: Equatable where T: Equatable {}
// expected-error@-1{{implementation of 'Equatable' cannot be automatically synthesized in an extension in a different file to the type}}

// rdar://problem/41852654

// There is a conformance to Equatable (or at least, one that implies Equatable)
// in the same file as the type, so the synthesis is okay. Both orderings are
// tested, to catch choosing extensions based on the order of the files, etc.
protocol ImplierMain: Equatable {}
struct ImpliedMain: ImplierMain {}
extension ImpliedOther: ImplierMain {}


// Hashable conformances that rely on a manual implementation of `hashValue`
// should produce a deprecation warning.
struct OldSchoolStruct: Hashable {
  static func ==(left: OldSchoolStruct, right: OldSchoolStruct) -> Bool {
    return true
  }
  var hashValue: Int { return 42 }
  // expected-warning@-1{{'Hashable.hashValue' is deprecated as a protocol requirement; conform type 'OldSchoolStruct' to 'Hashable' by implementing 'hash(into:)' instead}}
}
enum OldSchoolEnum: Hashable {
  case foo
  case bar

  static func ==(left: OldSchoolEnum, right: OldSchoolEnum) -> Bool {
    return true
  }
  var hashValue: Int { return 23 }
  // expected-warning@-1{{'Hashable.hashValue' is deprecated as a protocol requirement; conform type 'OldSchoolEnum' to 'Hashable' by implementing 'hash(into:)' instead}}
}
class OldSchoolClass: Hashable {
  static func ==(left: OldSchoolClass, right: OldSchoolClass) -> Bool {
    return true
  }
  var hashValue: Int { return -9000 }
  // expected-warning@-1{{'Hashable.hashValue' is deprecated as a protocol requirement; conform type 'OldSchoolClass' to 'Hashable' by implementing 'hash(into:)' instead}}
}

// However, it's okay to implement `hashValue` as long as `hash(into:)` is also
// provided.
struct MixedStruct: Hashable {
  static func ==(left: MixedStruct, right: MixedStruct) -> Bool {
    return true
  }
  func hash(into hasher: inout Hasher) {}
  var hashValue: Int { return 42 }
}
enum MixedEnum: Hashable {
  case foo
  case bar
  static func ==(left: MixedEnum, right: MixedEnum) -> Bool {
    return true
  }
  func hash(into hasher: inout Hasher) {}
  var hashValue: Int { return 23 }
}
class MixedClass: Hashable {
  static func ==(left: MixedClass, right: MixedClass) -> Bool {
    return true
  }
  func hash(into hasher: inout Hasher) {}
  var hashValue: Int { return -9000 }
}

// Ensure equatable and hashable works with weak/unowned properties as well
struct Foo: Equatable, Hashable {
    weak var foo: Bar?
    unowned var bar: Bar
}

class Bar {
   let bar: String

   init(bar: String) {
     self.bar = bar
   }
}

extension Bar: Equatable, Hashable {
  static func == (lhs: Bar, rhs: Bar) -> Bool {
        return lhs.bar == rhs.bar
  }

  func hash(into hasher: inout Hasher) {}
}

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected error produced: invalid redeclaration of 'hashValue'
// <unknown>:0: error: unexpected note produced: candidate has non-matching type '(Foo, Foo) -> Bool'
// <unknown>:0: error: unexpected note produced: candidate has non-matching type '<T> (Generic<T>, Generic<T>) -> Bool'
// <unknown>:0: error: unexpected note produced: candidate has non-matching type '(InvalidCustomHashable, InvalidCustomHashable) -> Bool'
// <unknown>:0: error: unexpected note produced: candidate has non-matching type '(EnumToUseBeforeDeclaration, EnumToUseBeforeDeclaration) -> Bool'
