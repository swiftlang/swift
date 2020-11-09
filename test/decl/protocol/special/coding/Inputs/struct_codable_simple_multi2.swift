// These are wrapped in a dummy function to avoid binding a global variable.
func foo() {
  // They should receive synthesized init(from:) and an encode(to:).
  let _ = SimpleStruct.init(from:)
  let _ = SimpleStruct.encode(to:)

  // The synthesized CodingKeys type should not be accessible from outside the
  // struct.
  let _ = SimpleStruct.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}
}

struct B {
  static let propertyName = A.propertyName
  
  struct Nest {
    static let propertyName = A.propertyName
  }
}
