// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000
// REQUIRES: tools-release,no_asan

let _ = (0...1).lazy.flatMap { // expected-error {{reasonable time}}
  a in (1...2).lazy.map { b in (a, b) }
}.filter {
  1 < $0 && $0 < $1 && $0 + $1 < 3
}
