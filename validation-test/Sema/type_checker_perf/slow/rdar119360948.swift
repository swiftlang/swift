// RUN: %target-typecheck-verify-swift -solver-scope-threshold=5000

func slow() {
  let _ = zip(0...4, 1...5).map {  // expected-error {{reasonable time}}
    (a, b) in (Double(b) / 5.0) * (Double(b) / 5.0) - (Double(a) / 5.0) * (Double(b) / 5.0)
  }
}
