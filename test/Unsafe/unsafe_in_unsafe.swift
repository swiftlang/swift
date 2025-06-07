// RUN: %target-typecheck-verify-swift

protocol P { }

struct X: @unsafe P { }

func returnMe(x: X) -> any P {
  x
}
