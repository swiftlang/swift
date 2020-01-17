// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

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
