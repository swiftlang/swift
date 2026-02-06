// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000
// REQUIRES: tools-release,no_asan
// REQUIRES: OS=macosx

struct Value {
  let debugDescription: String
}

func test(values: [[Value]]) -> String {
  values.map({ "[" + $0.map({ $0.debugDescription }).joined(separator: ", ") + "]" }).joined(separator: ", ")
}
