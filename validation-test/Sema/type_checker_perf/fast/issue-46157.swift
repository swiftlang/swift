// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100

// https://github.com/swiftlang/swift/issues/46157

func slow() {
  let _: (Double) -> Double = { x in x*x*x*x - 3*x*x*x + 2 }
}
