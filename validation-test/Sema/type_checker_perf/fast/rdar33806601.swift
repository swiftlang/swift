// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1 -solver-disable-shrink -disable-constraint-solver-performance-hacks -solver-enable-operator-designated-types
// REQUIRES: tools-release,no_asserts

class P {
  var x : Int = 0
  var y : Int = 1
}

func fn<T>(_ n: T) -> T where T : FloatingPoint { fatalError() }
func fn(_ n: Double) -> Double { return 0.0 }

let _: (P, P) -> Double = {
  (p : P, s : P)  -> Double in
  fn(Double((p.x - s.x) * (p.x - s.x) + (p.y - s.y) * (p.y - s.y)))
}
