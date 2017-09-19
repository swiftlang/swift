// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

struct Stringly {
  init(string: String) {}
  init(format: String, _ args: Any...) {}
  init(stringly: Stringly) {}
}

[Int](0..<1).map {
  print(Stringly(format: "%d: ", $0 * 2) + ["a", "b", "c", "d"][$0 * 2] + ",")
  // expected-error@-1 {{binary operator '+' cannot be applied to operands of type 'Stringly' and 'String'}}
  // expected-note@-2 {{expected an argument list of type '(String, String)'}}
}
