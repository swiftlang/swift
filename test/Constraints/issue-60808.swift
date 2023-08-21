// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/60808

protocol T {}
struct A: T {}

func test_switch<T>(arr: T) {
  if case _ as A = arr {}
}
