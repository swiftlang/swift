// {"kind":"emit-silgen","signature":"(anonymous namespace)::SILVerifier::_require(bool, llvm::Twine const&, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifier::requireABICompatibleFunctionTypes"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
// REQUIRES: OS=macosx
// REQUIRES: differentiable_programming
import _Differentiation
class a: Differentiable {
  @differentiable()
  func b(c: Double) -> Double {
  }
}
class d: a {
  @differentiable()
  override func b(c: Double) -> Double {
  }
}
