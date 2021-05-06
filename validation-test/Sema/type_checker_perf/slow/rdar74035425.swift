// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

struct Value {
  let debugDescription: String
}

func test(values: [[Value]]) -> String {
  // expected-error@+1 {{the compiler is unable to type-check this expression in reasonable time}}
  "[" + "" + "" + values.map({ "[" + $0.map({ $0.debugDescription }).joined(separator: ", ") + "" + "]" }).joined(separator: ", ") + "" + "]"
}

