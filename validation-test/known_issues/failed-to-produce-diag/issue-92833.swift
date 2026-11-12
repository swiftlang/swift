// rdar://188955987 (https://github.com/swiftlang/swift/issues/92833)
// RUN: %target-typecheck-verify-swift

struct State { var value: MissingType? }
// expected-error@-1 {{cannot find type 'MissingType' in scope}}

func reproduce(_ state: State) {
  _ = state.value?.member
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
}
