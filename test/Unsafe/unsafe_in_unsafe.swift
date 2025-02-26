// RUN: %target-typecheck-verify-swift -print-diagnostic-groups

protocol P { }

struct X: @unsafe P { }

func returnMe(x: X) -> any P {
  x
}
