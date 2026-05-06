// {"kind":"typecheck","original":"8f8ce502","signature":"swift::constraints::Solution::getFunctionArgApplyInfo(swift::constraints::ConstraintLocator*) const","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit","signatureNext":"GenericArgumentsMismatchFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a<b
  protocol c {
    associatedtype d
  }
  struct e
    func h<f: c>(_: f.Type
    ()->
      a<f      .d >)
      h(e
