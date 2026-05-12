// {"kind":"typecheck","original":"940008f2","signature":"swift::constraints::Solution::getFunctionArgApplyInfo(swift::constraints::ConstraintLocator*) const","signatureAssert":"Assertion failed: (!shouldHaveDirectCalleeOverload(call) && \"Should we have resolved a callee for this?\"), function getFunctionArgApplyInfo","signatureNext":"GenericArgumentsMismatchFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b, each c>(d: repeat (b inout [each c]) -> Void) {
  var f: [Any]  repeat
    (d(e
    f
