// RUN: %target-typecheck-verify-swift -verify-syntax-tree

func foo(x: Int?) {
  if let x {
    print(x)
  }
}
