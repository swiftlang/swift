// {"kind":"typecheck","signature":"swift::constraints::Solution::getFunctionArgApplyInfo(swift::constraints::ConstraintLocator*) const","signatureAssert":"Assertion failed: (!shouldHaveDirectCalleeOverload(call) && \"Should we have resolved a callee for this?\"), function getFunctionArgApplyInfo"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b, each c>(d : repeat(b)->each c) { repeat d(e = f
