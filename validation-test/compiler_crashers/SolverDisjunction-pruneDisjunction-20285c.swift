// {"kind":"typecheck","original":"fb9da832","signature":"swift::constraints::SolverDisjunction::pruneDisjunction(swift::constraints::ConstraintSystem&, swift::constraints::Constraint*, bool)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"SolverDisjunction::pruneDisjunctionIfNeeded"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(b: @()
func a(b: sending @autoclosure (
}
a {
