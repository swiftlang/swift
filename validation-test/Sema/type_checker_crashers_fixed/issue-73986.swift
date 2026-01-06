// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/73986

struct S {
  init(_: () -> some Any = { Nonexistent() }) {}
  // expected-error@-1 {{cannot find 'Nonexistent' in scope}}

  init<C>(other: C = { _ = 42; return Nonexistent() }) {}
  // expected-error@-1 {{cannot find 'Nonexistent' in scope}}
}

func test(x: some FixedWidthInteger = UNKNOWN_CONSTANT) {
  // expected-error@-1 {{cannot find 'UNKNOWN_CONSTANT' in scope}}
}
