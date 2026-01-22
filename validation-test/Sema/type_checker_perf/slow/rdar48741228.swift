// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000

func slow() {
  let _ = nil ?? 0 + nil ?? 0 - nil ?? 0 - nil ?? 0 // expected-error {{reasonable time}}
}
