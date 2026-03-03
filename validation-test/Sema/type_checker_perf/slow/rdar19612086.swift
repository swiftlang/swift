// RUN: %target-typecheck-verify-swift -solver-scope-threshold=50000 -solver-enable-prune-disjunctions
// REQUIRES: tools-release,no_asan

// Invalid expression: There is no + overload for Stringly

struct Stringly {
  init(string: String) {}
  init(format: String, _ args: Any...) {}
  init(stringly: Stringly) {}
}

struct rdar19612086 {
  let i = 0
  let x = 1.0

  var description : String {
    return "\(i)" + Stringly(format: "%.2f", x) +   // expected-error {{reasonable time}}
           "\(i+1)" + Stringly(format: "%.2f", x) +
           "\(i+2)" + Stringly(format: "%.2f", x) +
           "\(i+3)" + Stringly(format: "%.2f", x) +
           "\(i+4)" + Stringly(format: "%.2f", x) +
           "\(i+5)" + Stringly(format: "%.2f", x)
  }
}
