// {"kind":"emit-sil","signature":"swift::DifferentiableFunctionExtractInst::getExtracteeType(swift::SILValue, swift::NormalDifferentiableFunctionTypeComponent, swift::SILModule&)","signatureAssert":"Assertion failed: (fnTy->getDifferentiabilityKind() == DifferentiabilityKind::Reverse || fnTy->getDifferentiabilityKind() == DifferentiabilityKind::Normal || fnTy->getDifferentiabilityKind() == DifferentiabilityKind::Forward), function getExtracteeType","signatureNext":"DifferentiableFunctionExtractInst::DifferentiableFunctionExtractInst"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
// REQUIRES: OS=macosx
// REQUIRES: differentiable_programming
import _Differentiation
func a<b>(
  _ c: b,
  d: @differentiable(_linear) (b) -> b
) -> b {
  d(c)
}
func e<b>(
  f: b,
  g: @differentiable(_linear) (b) -> b
) {
  valueWithPullback(at: f) { c in
    a(c, d: g)
  }
}
