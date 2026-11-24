// rdar://161176872
// RUN: %target-typecheck-verify-swift

func s<T: BinaryInteger, U: FixedWidthInteger>(_: T, _: U) -> T {}
func s<T: BinaryInteger>(_: T, _: T) -> T {}

func f<T>(_: T, _: T, _: T) {}

func slow() {
  let _ = f(s(1, 1), s(1, 1), s(1, 1)) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

protocol P {}
protocol Q: P {}

extension P {
  func wrapping(_ error: (any Error)?) -> Self { self }
}
extension Q {
  func wrapping(_ error: (any Error)?) -> Self { self }
}

protocol A {}
protocol B {}

struct S: Q, A, B {}

extension A {
  static var failed: S { S() }
}
extension B {
  static var failed: S { S() }
}

func g(error: any Error) -> S {
  S.failed.wrapping(error) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
