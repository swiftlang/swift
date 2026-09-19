// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %s -disable-availability-checking

// REQUIRES: OS=macosx

struct EquatableInFutureMacOS {}

@available(macOS 99, *)
extension EquatableInFutureMacOS: Equatable {}

struct HashableInFutureMacOS {}

@available(macOS 99, *)
extension HashableInFutureMacOS: Hashable {}

struct EquatableObsoletedInMacOS10_9 {}

@available(macOS, introduced: 10.8, obsoleted: 10.9)
extension EquatableObsoletedInMacOS10_9: Equatable {}

struct EquatableUnavailableInMacOS {}

@available(macOS, unavailable)
extension EquatableUnavailableInMacOS: Equatable {}

struct CodableInFutureMacOS {}

@available(macOS 99, *)
extension CodableInFutureMacOS: Codable {}

struct StructWithEquatableInFutureMacOS: Equatable {
  // expected-error@-1 {{type 'StructWithEquatableInFutureMacOS' does not conform to protocol 'Equatable'}}
  // expected-note@-2 {{add stubs for conformance}}
  let member: EquatableInFutureMacOS
  // expected-note@-1 {{the conformance of stored property type 'EquatableInFutureMacOS' to 'Equatable' is only available in macOS 99 or newer, preventing synthesized conformance of 'StructWithEquatableInFutureMacOS' to 'Equatable'}}
}

struct StructWithHashableInFutureMacOS: Hashable {
  // expected-error@-1 {{type 'StructWithHashableInFutureMacOS' does not conform to protocol 'Hashable'}}
  // expected-error@-2 {{type 'StructWithHashableInFutureMacOS' does not conform to protocol 'Equatable'}}
  // expected-note@-3 {{add stubs for conformance}}
  let member: HashableInFutureMacOS
  // expected-note@-1 {{the conformance of stored property type 'HashableInFutureMacOS' to 'Hashable' is only available in macOS 99 or newer, preventing synthesized conformance of 'StructWithHashableInFutureMacOS' to 'Hashable'}}
  // expected-note@-2 {{the conformance of stored property type 'HashableInFutureMacOS' to 'Equatable' is only available in macOS 99 or newer, preventing synthesized conformance of 'StructWithHashableInFutureMacOS' to 'Equatable'}}
}

enum EnumWithEquatableInFutureMacOS: Equatable {
  // expected-error@-1 {{type 'EnumWithEquatableInFutureMacOS' does not conform to protocol 'Equatable'}}
  // expected-note@-2 {{add stubs for conformance}}
  case a(EquatableInFutureMacOS)
  // expected-note@-1 {{the conformance of associated value type 'EquatableInFutureMacOS' to 'Equatable' is only available in macOS 99 or newer, preventing synthesized conformance of 'EnumWithEquatableInFutureMacOS' to 'Equatable'}}
}

struct StructWithObsoletedEquatable: Equatable {
  // expected-error@-1 {{type 'StructWithObsoletedEquatable' does not conform to protocol 'Equatable'}}
  // expected-note@-2 {{add stubs for conformance}}
  let member: EquatableObsoletedInMacOS10_9
  // expected-note@-1 {{the conformance of stored property type 'EquatableObsoletedInMacOS10_9' to 'Equatable' is unavailable in macOS, preventing synthesized conformance of 'StructWithObsoletedEquatable' to 'Equatable'}}
}

struct StructWithEquatableUnavailableInMacOS: Equatable {
  // expected-error@-1 {{type 'StructWithEquatableUnavailableInMacOS' does not conform to protocol 'Equatable'}}
  // expected-note@-2 {{add stubs for conformance}}
  let member: EquatableUnavailableInMacOS
  // expected-note@-1 {{the conformance of stored property type 'EquatableUnavailableInMacOS' to 'Equatable' is unavailable in macOS, preventing synthesized conformance of 'StructWithEquatableUnavailableInMacOS' to 'Equatable'}}
}

struct StructWithCodableInFutureMacOS: Codable {
  // expected-error@-1 {{type 'StructWithCodableInFutureMacOS' does not conform to protocol 'Decodable'}}
  // expected-error@-2 {{type 'StructWithCodableInFutureMacOS' does not conform to protocol 'Encodable'}}
  let member: CodableInFutureMacOS
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because the conformance of 'CodableInFutureMacOS' to 'Decodable' is only available in macOS 99 or newer}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because the conformance of 'CodableInFutureMacOS' to 'Encodable' is only available in macOS 99 or newer}}
}

@available(macOS 99, *)
struct FutureStructWithEquatableInFutureMacOS: Equatable {
  let member: EquatableInFutureMacOS
}

@available(macOS 100, *)
struct LaterStructWithEquatableInFutureMacOS: Equatable {
  let member: EquatableInFutureMacOS
}

@available(macOS 99, *)
enum FutureEnumWithHashableInFutureMacOS: Hashable {
  case a(HashableInFutureMacOS)
}

@available(macOS 99, *)
struct FutureStructWithArrayOfHashableInFutureMacOS: Hashable {
  let member: [HashableInFutureMacOS]
}

struct StructWithEquatableInFutureMacOSInExtension {
  let member: EquatableInFutureMacOS
}

@available(macOS 99, *)
extension StructWithEquatableInFutureMacOSInExtension: Equatable {}

@available(macOS, unavailable)
struct UnavailableStructWithEquatableUnavailableInMacOS: Equatable {
  let member: EquatableUnavailableInMacOS
}

@available(macOS 99, *)
struct FutureStructWithCodableInFutureMacOS: Codable {
  let member: CodableInFutureMacOS
}

struct HashableUnavailableInMacOS {}

@available(macOS, unavailable)
extension HashableUnavailableInMacOS: Hashable {}

enum EnumWithCaseUnavailableInMacOS: Hashable {
  case available

  @available(macOS, unavailable)
  case unavailableInMacOS(HashableUnavailableInMacOS)
}
