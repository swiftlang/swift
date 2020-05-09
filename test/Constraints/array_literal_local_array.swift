// RUN: %target-typecheck-verify-swift

// SR-9611: Array type locally interfers with array literals.
struct Array { }

func foo() {
  _ = ["a", "b", "c"]
}
