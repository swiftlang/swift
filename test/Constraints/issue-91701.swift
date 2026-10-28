// RUN: %target-typecheck-verify-swift -solver-enable-promote-supertypes

// https://github.com/swiftlang/swift/issues/91701

func test() {
  let a = (x: Int64(0), y: Int64(0))
  let b = (x: Int64(1), y: Int64(1))
  let _: (p: (x: Int64, y: Int64), q: (x: Int64, y: Int64)) =
      (a.x, a.y) <= (b.x, b.y) ? (a, b) : (b, a)
}
