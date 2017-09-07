// RUN: %target-typecheck-verify-swift

// Simple structs where Codable conformance is added in a different-file
// extension should not derive conformance.
struct SimpleStruct { // expected-note {{did you mean 'init'?}}
  var x: Int
  var y: Double
  static var z: String = "foo"

  func foo() {
    // They should not receive a synthesized CodingKeys enum.
    let _ = SimpleStruct.CodingKeys.self // expected-error {{type 'SimpleStruct' has no member 'CodingKeys'}}
    let _ = SimpleStruct.CodingKeys.x // expected-error {{type 'SimpleStruct' has no member 'CodingKeys'}}
    let _ = SimpleStruct.CodingKeys.y // expected-error {{type 'SimpleStruct' has no member 'CodingKeys'}}
    let _ = SimpleStruct.CodingKeys.z // expected-error {{type 'SimpleStruct' has no member 'CodingKeys'}}
  }
}
