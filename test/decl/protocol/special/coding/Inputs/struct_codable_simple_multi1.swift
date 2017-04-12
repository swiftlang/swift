// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

import Foundation

// Simple structs with all Codable properties should get derived conformance to
// Codable.
struct SimpleStruct : Codable {
  var x: Int
  var y: Double
  static var z: String = "foo"

  // These lines have to be within the SimpleStruct type because CodingKeys
  // should be private.
  func foo() {
    // They should receive a synthesized CodingKeys enum.
    let _ = SimpleStruct.CodingKeys.self

    // The enum should have a case for each of the vars.
    let _ = SimpleStruct.CodingKeys.x
    let _ = SimpleStruct.CodingKeys.y

    // Static vars should not be part of the CodingKeys enum.
    let _ = SimpleStruct.CodingKeys.z // expected-error {{type 'SimpleStruct.CodingKeys' has no member 'z'}}
  }
}
