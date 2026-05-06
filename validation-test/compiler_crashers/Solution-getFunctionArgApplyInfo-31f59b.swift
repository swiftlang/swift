// {"kind":"typecheck","original":"40a86b54","signature":"swift::constraints::Solution::getFunctionArgApplyInfo(swift::constraints::ConstraintLocator*) const","signatureAssert":"Assertion failed: (!shouldHaveDirectCalleeOverload(call) && \"Should we have resolved a callee for this?\"), function getFunctionArgApplyInfo","signatureNext":"MissingArgumentsFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b, each c>(d: repeat ((b) -> each c) -> Void, e: repeat () -> each c) {
  repeat (d(e))
}
