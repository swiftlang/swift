// Simple classes with all Codable properties should get derived conformance to
// Codable.
class SimpleClass { // expected-note {{did you mean 'init'?}}
  var x: Int = 1
  var y: Double = .pi
  static var z: String = "foo"

  // These lines have to be within the SimpleClass type because
  // CodingKeys should be private.
  func foo() {
    // They should not get a CodingKeys type.
    let _ = SimpleClass.CodingKeys.self // expected-error {{type 'SimpleClass' has no member 'CodingKeys'}}
    let _ = SimpleClass.CodingKeys.x // expected-error {{type 'SimpleClass' has no member 'CodingKeys'}}
    let _ = SimpleClass.CodingKeys.y // expected-error {{type 'SimpleClass' has no member 'CodingKeys'}}
    let _ = SimpleClass.CodingKeys.z // expected-error {{type 'SimpleClass' has no member 'CodingKeys'}}
  }
}
