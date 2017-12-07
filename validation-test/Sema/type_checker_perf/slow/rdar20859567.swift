// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

struct Stringly {
  init(string: String) {}
  init(format: String, _ args: Any...) {}
  init(stringly: Stringly) {}
}

[Int](0..<1).map {
  // expected-error@-1 {{expression was too complex to be solved in reasonable time; consider breaking up the expression into distinct sub-expressions}}
  print(Stringly(format: "%d: ", $0 * 2 * 42) + ["a", "b", "c", "d", "e", "f", "g"][$0 * 2] + "," + "," + ",")
}
