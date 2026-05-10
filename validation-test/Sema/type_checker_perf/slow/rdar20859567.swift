// RUN: %target-typecheck-verify-swift -solver-scope-threshold=50000
// REQUIRES: tools-release,no_asan

struct Stringly {
  init(string: String) {}
  init(format: String, _ args: Any...) {}
  init(stringly: Stringly) {}
}

// Invalid expression: Missing overloads of +

[Int](0..<1).map {
  // expected-error@-1 {{reasonable time}}
  print(Stringly(format: "%d: ",
      $0 * 2 * 42 +
      $0 * 2 * 42 +
      $0 * 2 * 42
    ) + ["a", "b", "c", "d", "e", "f", "g"][$0 * 2] + "," + "," + ",")
}
