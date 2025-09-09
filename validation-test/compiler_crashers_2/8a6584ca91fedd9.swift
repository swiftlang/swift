// {"kind":"typecheck","signature":"swift::TypeChecker::typesSatisfyConstraint(swift::Type, swift::Type, bool, swift::constraints::ConstraintKind, swift::DeclContext*, bool*)","signatureAssert":"Assertion failed: (!type1->getRecursiveProperties().isSolverAllocated() && !type2->getRecursiveProperties().isSolverAllocated() && \"Cannot escape solver-allocated types into a nested ConstraintSystem\"), function typesSatisfyConstraint"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b>(c : repeat each b) { repeat !(d c
