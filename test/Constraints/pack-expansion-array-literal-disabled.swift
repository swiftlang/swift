// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

func toAny<each T>(_ t: repeat each T) -> [Any] {
  return [repeat each t]
  // expected-error@-1 {{value pack expansion can only appear inside a function argument list, tuple element, or as the expression of a for-in loop}}
}
