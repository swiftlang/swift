// {"kind":"typecheck","original":"1b96e3e3","signature":"swift::constraints::Solution::getFunctionArgApplyInfo(swift::constraints::ConstraintLocator*) const","signatureAssert":"Assertion failed: (!shouldHaveDirectCalleeOverload(call) && \"Should we have resolved a callee for this?\"), function getFunctionArgApplyInfo","signatureNext":"RValueTreatedAsLValueFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
0(&0)
