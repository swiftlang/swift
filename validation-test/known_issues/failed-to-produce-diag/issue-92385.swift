// rdar://187671494 (https://github.com/swiftlang/swift/issues/92385)
// RUN: %target-typecheck-verify-swift

func query<Result>(_ body: () -> Result) -> Result { body() }

func test() {
  let (a, b) = query { print(1) }
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
}
