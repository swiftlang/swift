// rdar://179776161 (https://github.com/swiftlang/swift/issues/89985)
// RUN: %target-typecheck-verify-swift

struct A: Equatable {
  let i: Int
}

func test() {
  let _: (A, A, (A, A) -> Bool) = [(A(i: 7), A(i: 3), ==)] // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
