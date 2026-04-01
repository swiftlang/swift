// {"kind":"typecheck","original":"bff75c6c","signature":"swift::constraints::Solution::getFunctionArgApplyInfo(swift::constraints::ConstraintLocator*) const","signatureAssert":"Assertion failed: (!shouldHaveDirectCalleeOverload(call) && \"Should we have resolved a callee for this?\"), function getFunctionArgApplyInfo","signatureNext":"InvalidPackExpansion::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b, each c>(d: repeat (repeat each b) -> each c) {
  repeat (d(.e))
}
