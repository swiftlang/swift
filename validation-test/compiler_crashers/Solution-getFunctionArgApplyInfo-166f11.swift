// {"kind":"typecheck","original":"e34f553a","signature":"swift::constraints::Solution::getFunctionArgApplyInfo(swift::constraints::ConstraintLocator*) const","signatureAssert":"Assertion failed: (!shouldHaveDirectCalleeOverload(call) && \"Should we have resolved a callee for this?\"), function getFunctionArgApplyInfo","signatureNext":"MissingOptionalUnwrapFailure::offerDefaultValueUnwrapFixIt"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b>(c: repeat each b?, d: repeat (each b) -> Bool) {
  repeat (d(c))
}
