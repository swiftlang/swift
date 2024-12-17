// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple -solver-expression-time-threshold=2
// REQUIRES: tools-release,no_asan

struct Tuple<each T> {
  func withBool() -> Tuple<repeat [each T], repeat (each T, Bool)> {}
}
let tuple = Tuple<Int>()
let _ = tuple // expected-error {{reasonable time}}
  .withBool()
  .withBool()
  .withBool()
  .withBool()
  .withBool()
  .withBool()
  .withBool()

