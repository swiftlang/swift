// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts
struct Stringly {
  init(string: String) {}
  init(format: String, _ args: Any...) {}
  init(stringly: Stringly) {}
}

struct rdar19612086 {
  let i = 0
  let x = 1.0

  var description : String {
    return "\(i)" + Stringly(format: "%.2f", x) +
           "\(i+1)" + Stringly(format: "%.2f", x) +
           "\(i+2)" + Stringly(format: "%.2f", x) +
           "\(i+3)" + Stringly(format: "%.2f", x) +
           "\(i+4)" + Stringly(format: "%.2f", x) +
           "\(i+5)" + Stringly(format: "%.2f", x)
    // expected-error@-1 {{reasonable time}}
  }
}
