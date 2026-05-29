// {"kind":"typecheck","original":"fb9da832","signature":"swift::constraints::SolverDisjunction::pruneDisjunction(swift::constraints::ConstraintSystem&, swift::constraints::Constraint*, bool)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"SolverDisjunction::pruneDisjunctionIfNeeded"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(b: @()
func a(b: sending @autoclosure (
}
a {
