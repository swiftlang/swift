// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -verify-ignore-unknown

// Simple structs with all Codable properties whose CodingKeys come from a
// typealias should get derived conformance to Codable.
struct SimpleStruct : Codable {
  var x: Int = 1
  var y: Double = .pi
  static var z: String = "foo"

  private typealias CodingKeys = A // expected-note {{'CodingKeys' declared here}}
  private typealias A = B
  private typealias B = C
  private typealias C = MyRealCodingKeys

  private enum MyRealCodingKeys : String, CodingKey {
    case x
    case y
  }
}

// They should receive synthesized init(from:) and an encode(to:).
let _ = SimpleStruct.init(from:)
let _ = SimpleStruct.encode(to:)

// The synthesized CodingKeys type should not be accessible from outside the
// struct.
let _ = SimpleStruct.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}

// Structs with CodingKeys which are typealiases that don't point to a valid
// nominal type should produce errors.
struct StructWithUndeclaredCodingKeys : Codable { // expected-error {{type 'StructWithUndeclaredCodingKeys' does not conform to protocol 'Decodable'}}
  // expected-error@-1 {{type 'StructWithUndeclaredCodingKeys' does not conform to protocol 'Encodable'}}
  private typealias CodingKeys = NonExistentType // expected-error {{cannot find type 'NonExistentType' in scope}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'CodingKeys' does not conform to CodingKey}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'CodingKeys' does not conform to CodingKey}}
}

// A typealias that resolves to a non-nominal type (tuple, function type) used
// to crash lookupEvaluatedCodingKeysEnum because getAnyNominal() returns null
// and the subsequent dyn_cast<EnumDecl> asserted on the null pointer. The
// compiler should diagnose the conformance failure rather than crash.
struct StructWithTupleCodingKeys : Codable { // expected-error {{type 'StructWithTupleCodingKeys' does not conform to protocol 'Decodable'}}
  // expected-error@-1 {{type 'StructWithTupleCodingKeys' does not conform to protocol 'Encodable'}}
  var x: Int = 1
  private typealias CodingKeys = (Int, Int)
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'CodingKeys' does not conform to CodingKey}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'CodingKeys' does not conform to CodingKey}}
}

struct StructWithFunctionTypeCodingKeys : Codable { // expected-error {{type 'StructWithFunctionTypeCodingKeys' does not conform to protocol 'Decodable'}}
  // expected-error@-1 {{type 'StructWithFunctionTypeCodingKeys' does not conform to protocol 'Encodable'}}
  var x: Int = 1
  private typealias CodingKeys = () -> Void
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'CodingKeys' does not conform to CodingKey}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'CodingKeys' does not conform to CodingKey}}
}

// An immutable 'let' with an initial value combined with a non-nominal
// CodingKeys typealias. The CheckCodableStoredPropertiesRequest diagnostic
// walks the CodingKeys enum; here lookupEvaluatedCodingKeysEnum returns null
// (the tuple has no nominal), so the request must bail out without crashing
// and without emitting a spurious "will not be decoded" warning for 'x'. Only
// the conformance failure should be diagnosed.
struct StructWithImmutableLetAndTupleCodingKeys : Codable { // expected-error {{type 'StructWithImmutableLetAndTupleCodingKeys' does not conform to protocol 'Decodable'}}
  // expected-error@-1 {{type 'StructWithImmutableLetAndTupleCodingKeys' does not conform to protocol 'Encodable'}}
  let x: Int = 1
  private typealias CodingKeys = (Int, Int)
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'CodingKeys' does not conform to CodingKey}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'CodingKeys' does not conform to CodingKey}}
}
