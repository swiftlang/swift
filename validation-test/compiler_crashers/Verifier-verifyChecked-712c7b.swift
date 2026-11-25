// {"kind":"typecheck","signature":"(anonymous namespace)::Verifier::verifyChecked(swift::AbstractClosureExpr*)","signatureNext":"Verifier::dispatchVisitPost"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
// REQUIRES: differentiable_programming
import _Differentiation
protocol P: Differentiable {
  @differentiable(reverse)
  func g(_ x: Float) -> Float
}
func f(_ a: Int?, _ b: Int) -> Int {
  struct Local: P {
    @differentiable(reverse)
    func g(_ x: Float) -> Float { x }
  }
  return a ?? b
}
