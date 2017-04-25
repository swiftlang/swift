// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// REQUIRES: objc_interop

import Foundation

// Classes with computed members should get synthesized conformance to Codable,
// but their lazy and computed members should be skipped as part of the
// synthesis.
class ClassWithComputedMembers : Codable {
  var x: Int = 1
  lazy var y: Double = .pi
  var z: String {
    return "foo"
  }

  // These lines have to be within the ClassWithComputedMembers type because
  // CodingKeys should be private.
  func foo() {
    // They should receive a synthesized CodingKeys enum.
    let _ = ClassWithComputedMembers.CodingKeys.self

    // The enum should have a case for each of the vars.
    let _ = ClassWithComputedMembers.CodingKeys.x

    // Lazy vars should not be part of the CodingKeys enum.
    let _ = ClassWithComputedMembers.CodingKeys.y // expected-error {{type 'ClassWithComputedMembers.CodingKeys' has no member 'y'}}

    // Computed vars should not be part of the CodingKeys enum.
    let _ = ClassWithComputedMembers.CodingKeys.z // expected-error {{type 'ClassWithComputedMembers.CodingKeys' has no member 'z'}}
  }
}

// These are wrapped in a dummy function to avoid binding a global variable.
func foo() {
  // They should receive synthesized init(from:) and an encode(to:).
  let _ = ClassWithComputedMembers.init(from:)
  let _ = ClassWithComputedMembers.encode(to:)

  // The synthesized CodingKeys type should not be accessible from outside the
  // class.
  let _ = ClassWithComputedMembers.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}
}
