// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

let _ = (0...1).lazy.flatMap {
  a in (1...2).lazy.map { b in (a, b) }
}.filter {
  // expected-error@-1 {{reasonable time}}
  1 < $0 && $0 < $1 && $0 + $1 < 3
}
