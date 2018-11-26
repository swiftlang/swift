// RUN: %target-typecheck-verify-swift

func foo() {
  // Okay to reference a type declared later in the same function.
  _ = Visitor()
  struct Visitor { }
}
