// rdar://186445747 (https://github.com/swiftlang/swift/issues/91906)
// RUN: %target-typecheck-verify-swift

protocol P {}
protocol Q : P {}

struct S {
  var layer: any P
}

func f(_ x: [S]) -> (any Q)? {
  x.first?.layer as (any Q)?
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
}
