// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

func test(strings: [String]) {
  for string in strings {
    let _ = string.split(omittingEmptySubsequences: false) { $0 == "C" || $0 == "D" || $0 == "H" || $0 == "S"}
  }
}
