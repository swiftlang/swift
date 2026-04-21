// {"kind":"typecheck","original":"0bee11b5","signature":"swift::constraints::Solution::getFunctionArgApplyInfo(swift::constraints::ConstraintLocator*) const","signatureAssert":"Assertion failed: (!shouldHaveDirectCalleeOverload(call) && \"Should we have resolved a callee for this?\"), function getFunctionArgApplyInfo","signatureNext":"RValueTreatedAsLValueFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b> {
  var c: d
  var wrappedValue: b {
    @a var e = &0.0
  }
}
