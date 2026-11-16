// {"kind":"emit-sil","signature":"(anonymous namespace)::DifferentiationTransformer::processDifferentiableFunctionInst(swift::DifferentiableFunctionInst*)","signatureAssert":"Assertion failed: (wasEmplace && \"Expect all 'afdToSILFn' cache entries to be \" \"filled at once on the first access attempt\"), function getUnwrappedCurryThunkFunction","signatureNext":"Differentiation::run"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
// REQUIRES: OS=macosx
// REQUIRES: differentiable_programming
import _Differentiation
open class a<b> {
  public func c(g: Float) -> Float {
  }
}
@inlinable
func d<e>(
  g: a<e>
) {
  let f: @differentiable(reverse) (Float) -> Float = g.c
}
