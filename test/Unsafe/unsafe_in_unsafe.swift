// RUN: %target-typecheck-verify-swift -enable-experimental-feature AllowUnsafeAttribute -print-diagnostic-groups

// REQUIRES: swift_feature_AllowUnsafeAttribute


protocol P { }

struct X: @unsafe P { }

func returnMe(x: X) -> any P {
  x
}
