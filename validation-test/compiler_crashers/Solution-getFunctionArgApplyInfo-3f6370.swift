// {"kind":"typecheck","original":"98525e94","signature":"swift::constraints::Solution::getFunctionArgApplyInfo(swift::constraints::ConstraintLocator*) const","signatureAssert":"Assertion failed: (!shouldHaveDirectCalleeOverload(call) && \"Should we have resolved a callee for this?\"), function getFunctionArgApplyInfo","signatureNext":"InvalidUseOfAddressOf::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b, each c>(d: b, e: repeat (b, each c) -> Void) {
  repeat (e(&d))
}
