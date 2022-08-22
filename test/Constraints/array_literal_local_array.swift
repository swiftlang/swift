// RUN: %target-typecheck-verify-swift

/// https://github.com/apple/swift/issues/52057
/// `Array` type locally interferes with array literals

struct Array { }

func foo() {
  _ = ["a", "b", "c"]
}
