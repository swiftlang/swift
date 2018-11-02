// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

class SimpleClass : Codable {
  var x: Int = 1
  var y: Double = .pi
  static var z: String = "foo"
}

// The synthesized CodingKeys type should not be accessible from outside the
// class.
let _ = SimpleClass.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}

// Classes which inherit from classes that are codable should synthesize Codable
// conformance as well.
class SimpleChildClass : SimpleClass {
  var w: Bool = true

  // NOTE: These tests will fail in the future as Codable classes are updated
  //       to derive Codable conformance instead of inheriting their
  //       superclass's. Classes currently inherit their parent's Codable
  //       conformance and we never get the chance to derive a CodingKeys
  //       type, nor overridden methods.

  // These lines have to be within the SimpleChildClass type because
  // CodingKeys should be private.
  func foo() {
    // They should receive a synthesized CodingKeys enum.
    // NOTE: This expected error will need to be removed in the future.
    let _ = SimpleChildClass.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}

    // The enum should have a case for each of the vars.
    // NOTE: This expected error will need to be removed in the future.
    let _ = SimpleChildClass.CodingKeys.w // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}

    // Inherited vars should not be part of the CodingKeys enum.
    // NOTE: This expected error will need to be updated in the future.
    //       Should be `type 'SimpleClass.CodingKeys' has no member 'x'`
    let _ = SimpleChildClass.CodingKeys.x // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}

    // NOTE: This expected error will need to be updated in the future.
    //       Should be `type 'SimpleClass.CodingKeys' has no member 'y'`
    let _ = SimpleChildClass.CodingKeys.y // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}
  }
}

// They should receive synthesized init(from:) and an encode(to:).
let _ = SimpleChildClass.init(from:)
let _ = SimpleChildClass.encode(to:)

// The synthesized CodingKeys type should not be accessible from outside the
// class.
let _ = SimpleChildClass.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}

// Check access level issues around 'private'.
private class PrivateClass: Codable {
  var x: Int = 1
}

private class PrivateClassChild: PrivateClass {}

_ = PrivateClass.init(from:)
_ = PrivateClass.encode(to:)
_ = PrivateClassChild.init(from:)
_ = PrivateClassChild.encode(to:)

_ = PrivateClass.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}

// Check access level issues around 'open'.
open class OpenClass: Codable {
  var x: Int = 1
}
open class OpenClassChild: OpenClass {}

_ = OpenClass.init(from:)
_ = OpenClass.encode(to:)
_ = OpenClassChild.init(from:)
_ = OpenClassChild.encode(to:)

_ = OpenClass.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}
