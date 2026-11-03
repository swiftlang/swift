// {"extraArgs":["-language-mode","6","-experimental-allow-module-with-compiler-errors"],"kind":"emit-sil","original":"f44f1ca7","signature":"swift::PartitionOpEvaluator<swift::regionanalysisimpl::BlockPartitionState::recomputeExitFromEntry(swift::regionanalysisimpl::PartitionOpTranslator&)::ComputeEvaluator>::apply(swift::PartitionOp const&)","signatureAssert":"Assertion failed: (p.isTrackingElement(op.getOpArg1()) && \"Require PartitionOp's argument should already be tracked\"), function apply","signatureNext":"BlockPartitionState::recomputeExitFromEntry"}
// RUN: not --crash %target-swift-frontend -emit-sil -language-mode 6 -experimental-allow-module-with-compiler-errors %s
func a() {
  let b =
    c
  func c(d: Int) {
    d <= 0 ? e : 1
  }
  var e = 0
}
