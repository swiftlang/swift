// {"kind":"typecheck","original":"123f98be","signature":"swift::constraints::Solution::getFunctionArgApplyInfo(swift::constraints::ConstraintLocator*) const","signatureAssert":"Assertion failed: (!shouldHaveDirectCalleeOverload(call) && \"Should we have resolved a callee for this?\"), function getFunctionArgApplyInfo","signatureNext":"MissingArgumentsFailure::diagnoseClosure"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<each b> {
  var c: (repeat ((each b) -> String) -> Int) = repeat (each c) {
  }
}
