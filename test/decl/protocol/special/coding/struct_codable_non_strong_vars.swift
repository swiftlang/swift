// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// Structs with Codable properties (with non-strong ownership) should get
// derived conformance to Codable.
struct NonStrongStruct : Codable {
  class NestedClass : Codable {
    init() {}
  }

  weak var x: NestedClass? = NestedClass()
  unowned var y: NestedClass = NestedClass()
  static var z: String = "foo"

  // These lines have to be within the NonStrongStruct type because CodingKeys
  // should be private.
  func foo() {
    // They should receive a synthesized CodingKeys enum.
    let _ = NonStrongStruct.CodingKeys.self

    // The enum should have a case for each of the vars.
    let _ = NonStrongStruct.CodingKeys.x
    let _ = NonStrongStruct.CodingKeys.y

    // Static vars should not be part of the CodingKeys enum.
    let _ = NonStrongStruct.CodingKeys.z // expected-error {{type 'NonStrongStruct.CodingKeys' has no member 'z'}}
  }
}

// They should receive synthesized init(from:) and an encode(to:).
let _ = NonStrongStruct.init(from:)
let _ = NonStrongStruct.encode(to:)

// The synthesized CodingKeys type should not be accessible from outside the
// class.
let _ = NonStrongStruct.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}
