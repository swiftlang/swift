// rdar://186445544 (https://github.com/swiftlang/swift/issues/91905)
// RUN: %target-typecheck-verify-swift

protocol P {}
protocol Q: P {}

struct S: P {
  var layer: any P { self }
}

func test(_ s: [S]) -> (any Q)? {
  s.first?.layer // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
