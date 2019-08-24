// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// Classes with Codable properties (with non-strong ownership) should get
// derived conformance to Codable.
class NonStrongClass : Codable {
  class NestedClass : Codable {
    init() {}
  }

  weak var x: NestedClass? = NestedClass()
  // expected-warning@-1 {{instance will be immediately deallocated because property 'x' is 'weak'}}
  // expected-note@-2 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-3 {{'x' declared here}}

  unowned var y: NestedClass = NestedClass()
  // expected-warning@-1 {{instance will be immediately deallocated because property 'y' is 'unowned'}}
  // expected-note@-2 {{a strong reference is required to prevent the instance from being deallocated}}
  // expected-note@-3 {{'y' declared here}}

  static var z: String = "foo"

  // These lines have to be within the NonStrongClass type because CodingKeys
  // should be private.
  func foo() {
    // They should receive a synthesized CodingKeys enum.
    let _ = NonStrongClass.CodingKeys.self

    // The enum should have a case for each of the vars.
    let _ = NonStrongClass.CodingKeys.x
    let _ = NonStrongClass.CodingKeys.y

    // Static vars should not be part of the CodingKeys enum.
    let _ = NonStrongClass.CodingKeys.z // expected-error {{type 'NonStrongClass.CodingKeys' has no member 'z'}}
  }
}

// They should receive synthesized init(from:) and an encode(to:).
let _ = NonStrongClass.init(from:)
let _ = NonStrongClass.encode(to:)

// The synthesized CodingKeys type should not be accessible from outside the
// class.
let _ = NonStrongClass.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}
