// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// cases without associated values should not generate separate CodingKeys
// implementations, but use a shared one instead.
enum SimpleEnum : Codable {
  case x
  case y
  case z
}

let _ = SimpleEnum.XCodingKeys.self // expected-error {{'XCodingKeys' is inaccessible due to 'private' protection level}}
let _ = SimpleEnum.YCodingKeys.self // expected-error {{'YCodingKeys' is inaccessible due to 'private' protection level}}
let _ = SimpleEnum.ZCodingKeys.self // expected-error {{'ZCodingKeys' is inaccessible due to 'private' protection level}}
let _ = SimpleEnum.__empty__CodingKeys.self // expected-error {{'__empty__CodingKeys' is inaccessible due to 'private' protection level}}
