// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %s -disable-availability-checking

// Synthesized witnesses cannot be guarded with an `if #available(...)` query,
// so a derived conformance can only rely on the conformances of its members
// when the availability of the conformance declaration satisfies every
// restriction on them. The availability of a member conformance must not
// affect which conformances the compiler derives when availability checking is
// disabled, so the second run expects no diagnostics at all.

struct UnavailableEquatable {}

@available(*, unavailable)
extension UnavailableEquatable: Equatable {}

struct UnavailableHashable {}

@available(*, unavailable, message: "use something else")
extension UnavailableHashable: Hashable {}

struct UnavailableComparable {}

@available(*, unavailable)
extension UnavailableComparable: Comparable {
  static func < (lhs: Self, rhs: Self) -> Bool { false }
}

struct UnavailableCodable {}

@available(*, unavailable)
extension UnavailableCodable: Codable {}

struct StructWithUnavailableEquatable: Equatable { // expected-error {{type 'StructWithUnavailableEquatable' does not conform to protocol 'Equatable'}}
  // expected-note@-1 {{add stubs for conformance}}
  let member: UnavailableEquatable
  // expected-note@-1 {{the conformance of stored property type 'UnavailableEquatable' to 'Equatable' is unavailable, preventing synthesized conformance of 'StructWithUnavailableEquatable' to 'Equatable'}}
}

struct StructWithUnavailableHashable: Hashable {
  // expected-error@-1 {{type 'StructWithUnavailableHashable' does not conform to protocol 'Hashable'}}
  // expected-error@-2 {{type 'StructWithUnavailableHashable' does not conform to protocol 'Equatable'}}
  // expected-note@-3 {{add stubs for conformance}}
  let member: UnavailableHashable
  // expected-note@-1 {{the conformance of stored property type 'UnavailableHashable' to 'Hashable' is unavailable: use something else, preventing synthesized conformance of 'StructWithUnavailableHashable' to 'Hashable'}}
  // expected-note@-2 {{the conformance of stored property type 'UnavailableHashable' to 'Equatable' is unavailable: use something else, preventing synthesized conformance of 'StructWithUnavailableHashable' to 'Equatable'}}
}

struct StructWithUnavailableCodable: Codable {
  // expected-error@-1 {{type 'StructWithUnavailableCodable' does not conform to protocol 'Decodable'}}
  // expected-error@-2 {{type 'StructWithUnavailableCodable' does not conform to protocol 'Encodable'}}
  let member: UnavailableCodable
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because the conformance of 'UnavailableCodable' to 'Decodable' is unavailable}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because the conformance of 'UnavailableCodable' to 'Encodable' is unavailable}}
}

// MARK: - Enums

enum EnumWithUnavailableEquatable: Equatable { // expected-error {{type 'EnumWithUnavailableEquatable' does not conform to protocol 'Equatable'}}
  // expected-note@-1 {{add stubs for conformance}}
  case a(UnavailableEquatable)
  // expected-note@-1 {{the conformance of associated value type 'UnavailableEquatable' to 'Equatable' is unavailable, preventing synthesized conformance of 'EnumWithUnavailableEquatable' to 'Equatable'}}
}

enum EnumWithUnavailableHashable: Hashable {
  // expected-error@-1 {{type 'EnumWithUnavailableHashable' does not conform to protocol 'Hashable'}}
  // expected-error@-2 {{type 'EnumWithUnavailableHashable' does not conform to protocol 'Equatable'}}
  // expected-note@-3 {{add stubs for conformance}}
  case a(UnavailableHashable)
  // expected-note@-1 {{the conformance of associated value type 'UnavailableHashable' to 'Hashable' is unavailable: use something else, preventing synthesized conformance of 'EnumWithUnavailableHashable' to 'Hashable'}}
  // expected-note@-2 {{the conformance of associated value type 'UnavailableHashable' to 'Equatable' is unavailable: use something else, preventing synthesized conformance of 'EnumWithUnavailableHashable' to 'Equatable'}}
}

enum EnumWithUnavailableComparable: Comparable {
  // expected-error@-1 {{type 'EnumWithUnavailableComparable' does not conform to protocol 'Comparable'}}
  // expected-error@-2 {{type 'EnumWithUnavailableComparable' does not conform to protocol 'Equatable'}}
  // expected-note@-3 {{add stubs for conformance}}
  case a(UnavailableComparable)
  // expected-note@-1 {{the conformance of associated value type 'UnavailableComparable' to 'Comparable' is unavailable, preventing synthesized conformance of 'EnumWithUnavailableComparable' to 'Comparable'}}
  // expected-note@-2 {{the conformance of associated value type 'UnavailableComparable' to 'Equatable' is unavailable, preventing synthesized conformance of 'EnumWithUnavailableComparable' to 'Equatable'}}
}

enum EnumWithUnavailableCodable: Codable {
  // expected-error@-1 {{type 'EnumWithUnavailableCodable' does not conform to protocol 'Decodable'}}
  // expected-error@-2 {{type 'EnumWithUnavailableCodable' does not conform to protocol 'Encodable'}}
  case a(UnavailableCodable)
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because the conformance of 'UnavailableCodable' to 'Decodable' is unavailable}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because the conformance of 'UnavailableCodable' to 'Encodable' is unavailable}}
}

struct UnavailableEquatableRawValue: ExpressibleByIntegerLiteral {
  init(integerLiteral value: Int) {}
}

@available(*, unavailable)
extension UnavailableEquatableRawValue: Equatable {
  static func == (lhs: Self, rhs: Self) -> Bool { true }
}

enum EnumWithUnavailableEquatableRawType: UnavailableEquatableRawValue {
  // expected-error@-1 {{'EnumWithUnavailableEquatableRawType' declares raw type 'UnavailableEquatableRawValue', but does not conform to RawRepresentable and conformance could not be synthesized}}
  // expected-note@-2 {{the conformance of raw type 'UnavailableEquatableRawValue' to 'Equatable' is unavailable, preventing synthesized conformance of 'EnumWithUnavailableEquatableRawType' to 'RawRepresentable'}}
  // expected-note@-3 {{add stubs for conformance}}
  case a = 1
}

// The conformance of 'Array' to 'Equatable' depends on the conformance of its
// element type.
struct StructWithArrayOfUnavailableEquatable: Equatable {
  // expected-error@-1 {{type 'StructWithArrayOfUnavailableEquatable' does not conform to protocol 'Equatable'}}
  // expected-note@-2 {{add stubs for conformance}}
  let member: [UnavailableEquatable]
  // expected-note@-1 {{the conformance of stored property type '[UnavailableEquatable]' to 'Equatable' is unavailable, preventing synthesized conformance of 'StructWithArrayOfUnavailableEquatable' to 'Equatable'}}
}

@available(*, unavailable)
struct UnavailableStructWithUnavailableEquatable: Equatable {
  let member: UnavailableEquatable
}

@available(*, unavailable)
struct UnavailableStructWithUnavailableHashable: Hashable {
  let member: UnavailableHashable
}

@available(*, unavailable)
enum UnavailableEnumWithUnavailableEquatable: Equatable {
  case a(UnavailableEquatable)
}

@available(*, unavailable)
enum UnavailableEnumWithUnavailableHashable: Hashable {
  case a(UnavailableHashable)
}

@available(*, unavailable)
struct UnavailableStructWithUnavailableCodable: Codable {
  let member: UnavailableCodable
}

struct StructWithUnavailableEquatableInExtension {
  let member: UnavailableEquatable
}

@available(*, unavailable)
extension StructWithUnavailableEquatableInExtension: Equatable {}

enum EnumWithUnavailableCase: Hashable {
  case available
  @available(*, unavailable)
  case unavailable(UnavailableHashable)
}

enum ComparableEnumWithUnavailableCase: Comparable {
  case available
  @available(*, unavailable)
  case unavailable(UnavailableComparable)
}

enum CodableEnumWithUnavailableCase: Codable {
  case available
  @available(*, unavailable)
  case unavailable(UnavailableCodable)
}

struct AvailableEquatable {}

extension AvailableEquatable: Equatable {}

struct StructWithAvailableEquatable: Equatable {
  let member: AvailableEquatable
}

enum EnumWithAvailableEquatable: Equatable {
  case a(AvailableEquatable)
}
