// rdar://186832337 (https://github.com/swiftlang/swift/issues/92022)
// RUN: %target-typecheck-verify-swift

let _: (Int) -> Int? = { 1 }
// expected-error@-1 {{contextual type for closure argument list expects 1 argument, which cannot be implicitly ignored}}

let (_, _): ((Int) -> Int?, (Int) -> Int?) = Bool.random() ? (
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
  { 1 },
  { _ in 2 }
) : (
  { _ in 1 },
  { _ in 2 }
)
