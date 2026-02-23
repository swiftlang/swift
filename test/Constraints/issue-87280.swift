// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/87280

enum E: Equatable {
  case e(Double)
}

func bar<T, U>(_: T, _: (T, U) -> Bool, _: U) {}

func foo(_ x: E) {
  bar(x, { $0 == $1 }, .e(1 * 1 * 1))
}
