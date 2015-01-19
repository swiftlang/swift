// RUN: %target-parse-verify-swift

// An inout parameter can be captured.
func foo(inout x: Int) {
  func bar() -> Int {
    return x
  }
}
