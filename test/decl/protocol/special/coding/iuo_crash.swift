// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// Crash due to codable synthesis with implicitly unwrapped optionals of
// ill-formed types.
// rdar://problem/60985179
struct X: Codable {  // expected-error 2{{type 'X' does not conform to protocol}}
  var c: Undefined! // expected-error{{cannot find type 'Undefined' in scope}}
  // expected-note @-1{{does not conform to}}
  // expected-note @-2{{does not conform to}}
}
