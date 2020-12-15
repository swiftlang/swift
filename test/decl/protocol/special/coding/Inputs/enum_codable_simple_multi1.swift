// RUN: %target-typecheck-verify-swift

// Simple enums with all Codable properties should get derived conformance to
// Codable.
enum SimpleEnum : Codable {
  case a(x: Int, y: Double)
  case b(z: String)

  // These lines have to be within the SimpleEnum type because CodingKeys
  // should be private.
  func foo() {
    // They should receive a synthesized CodingKeys enum.
    let _ = SimpleEnum.CodingKeys.self
    let _ = SimpleEnum.ACodingKeys.self
    let _ = SimpleEnum.BCodingKeys.self

    // The enum should have a case for each of the cases.
    let _ = SimpleEnum.CodingKeys.a
    let _ = SimpleEnum.CodingKeys.b

    // The enum should have a case for each of the vars.
    let _ = SimpleEnum.ACodingKeys.x
    let _ = SimpleEnum.ACodingKeys.y

    let _ = SimpleEnum.BCodingKeys.z
  }
}
