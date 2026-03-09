// {"kind":"typecheck","original":"052e2764","signature":"swift::TypeChecker::typesSatisfyConstraint(swift::Type, swift::Type, bool, swift::constraints::ConstraintKind, swift::DeclContext*, bool*)","signatureAssert":"Assertion failed: (!type1->getRecursiveProperties().isSolverAllocated() && !type2->getRecursiveProperties().isSolverAllocated() && \"Cannot escape solver-allocated types into a nested ConstraintSystem\"), function typesSatisfyConstraint"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a {
  var b: String {
    {
      [c = b.count] in
      .prefix(c).joined
    }
  }
}
