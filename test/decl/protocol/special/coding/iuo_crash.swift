// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// Crash due to codable synthesis with implicitly unwrapped optionals of
// ill-formed types.
// rdar://problem/60985179
struct X: Codable {
  var c: Undefined! // expected-error{{cannot find type 'Undefined' in scope}}
}
