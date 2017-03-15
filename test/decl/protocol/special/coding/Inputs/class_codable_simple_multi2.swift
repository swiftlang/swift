// These are wrapped in a dummy function to avoid binding a global variable.
func foo() {
  // They should receive synthesized init(from:) and an encode(to:).
  let _ = SimpleClass.init(from:)
  let _ = SimpleClass.encode(to:)

  // The synthesized CodingKeys type should not be accessible from outside the
  // class.
  let _ = SimpleClass.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}
}
