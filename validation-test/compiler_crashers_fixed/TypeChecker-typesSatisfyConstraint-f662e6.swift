// {"frontendArgs":["-typecheck"],"kind":"custom","signature":"swift::TypeChecker::typesSatisfyConstraint(swift::Type, swift::Type, bool, swift::constraints::ConstraintKind, swift::DeclContext*, bool*)","signatureAssert":"Assertion failed: (!type1->getRecursiveProperties().isSolverAllocated() && !type2->getRecursiveProperties().isSolverAllocated() && \"Cannot escape solver-allocated types into a nested ConstraintSystem\"), function typesSatisfyConstraint","signatureNext":"compareThrownErrorsForSubtyping"}
// RUN: not %target-swift-frontend -typecheck %s
func a() -> b {
  Result {
    throw <#expression#>
  }
}
