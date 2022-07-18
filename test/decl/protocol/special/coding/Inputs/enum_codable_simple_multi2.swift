// These are wrapped in a dummy function to avoid binding a global variable.
func foo() {
  // They should receive synthesized init(from:) and an encode(to:).
  let _ = SimpleEnum.init(from:)
  let _ = SimpleEnum.encode(to:)

  // The synthesized CodingKeys type should not be accessible from outside the
  // enum.
  let _ = SimpleEnum.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}
  let _ = SimpleEnum.ACodingKeys.self // expected-error {{'ACodingKeys' is inaccessible due to 'private' protection level}}
  let _ = SimpleEnum.BCodingKeys.self // expected-error {{'BCodingKeys' is inaccessible due to 'private' protection level}}
}
