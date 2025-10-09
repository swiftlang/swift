// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000

// https://github.com/swiftlang/swift/issues/49476

// This is invalid, because offset and index have mismatched types

func slow() {
  let offset: Double = 5.0
  let index: Int = 10
  let angle = (180.0 - offset + index * 5.0) * .pi / 18
  // expected-error@-1{{reasonable time}}
}
