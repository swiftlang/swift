// {"kind":"typecheck","original":"1aac3681","signature":"swift::constraints::Solution::getFunctionArgApplyInfo(swift::constraints::ConstraintLocator*) const","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit","signatureNext":"NonClassTypeToAnyObjectConversionFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b {
  init(wrappedValue: b  AnyObject)
  var wrappedValue: b {
    @a var c = (0
    ""
