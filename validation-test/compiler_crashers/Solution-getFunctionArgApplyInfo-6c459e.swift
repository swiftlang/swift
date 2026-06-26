// {"kind":"typecheck","original":"88297a3e","signature":"swift::constraints::Solution::getFunctionArgApplyInfo(swift::constraints::ConstraintLocator*) const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"RValueTreatedAsLValueFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a()
& &a
