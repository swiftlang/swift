// {"kind":"emit-sil","extraArgs":["-language-mode", "5"],"signature":"swift::ConstExprStepEvaluator::tryEvaluateOrElseMakeEffectsNonConstant(llvm::ilist_iterator<llvm::ilist_detail::compute_node_options<swift::SILInstruction>::type, false, false>)","signatureAssert":"Assertion failed: (errorVal.isUnknown()), function isFailStopError","signatureNext":"constantFold"}
// RUN: not --crash %target-swift-frontend -emit-sil -language-mode 5 %s
struct a: StringInterpolationProtocol {
  var b: UInt16
  @_semantics("constant_evaluable")
  init(literalCapacity: Int, interpolationCount: Int) {
  }
  func appendLiteral(_: String) {
  }
  func appendInterpolation(_: Int64, x: Int) {
  }
}
struct c: ExpressibleByStringInterpolation {
  @_semantics("oslog.message.init_interpolation")
  init(stringInterpolation: a)
  init(stringLiteral: String) {
  }
}
func d(_: c) {
}
func e(f: [Int64], g: Bool) {
  for h in f {
    if g {
      d("\(h, x: 0)")
    }
  }
}
