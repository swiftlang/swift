// {"kind":"emit-sil","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifier::visitSILInstruction"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
// REQUIRES: OS=macosx
// REQUIRES: differentiable_programming
import _Differentiation
protocol a: Differentiable {
}
@propertyWrapper
struct b<c> {
  var wrappedValue: c
}
struct d<c>: Differentiable {
  @b var e: c
  func f(_: c) -> c {
  }
  var g: c {
    get {
    }
    _modify {
      yield &e
    }
  }
}
@differentiable()
func h<c: a>(i: d<c>, k: c) -> c {
  var j = i
  j.g = k
  return j.f(k)
}
