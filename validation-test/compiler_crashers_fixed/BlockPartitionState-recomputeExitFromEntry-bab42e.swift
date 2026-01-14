// {"kind":"emit-sil","languageMode":6,"signature":"swift::regionanalysisimpl::BlockPartitionState::recomputeExitFromEntry(swift::regionanalysisimpl::PartitionOpTranslator&)","signatureAssert":"Assertion failed: (p.isTrackingElement(op.getOpArg1()) && \"Require PartitionOp's argument should already be tracked\"), function apply"}
// RUN: not %target-swift-frontend -emit-sil -swift-version 6 %s
func a<b>(d: b) {
  var e = 0
  let d: @convention(c) () -> Void = {
    e
  }
}
