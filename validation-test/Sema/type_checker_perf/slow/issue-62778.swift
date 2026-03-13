// RUN: %target-typecheck-verify-swift -solver-scope-threshold=10000
// This also fails with the default limit.

// https://github.com/swiftlang/swift/issues/62778

// Invalid expression, because there is no (UInt, Int) overload of |.

func slow() {
  let pieces = [1, 2, 3, 4]
  let _ = (UInt(pieces[0]) << 24) | (UInt(pieces[1]) << 16) | (UInt(pieces[2]) << 8) | pieces[3]
  // expected-error@-1 {{reasonable time}}
}
