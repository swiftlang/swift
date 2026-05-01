// {"kind":"typecheck","original":"681e53ca","signature":"swift::constraints::ConstraintSystem::recordPackElementExpansion(swift::PackElementExpr*, swift::PackExpansionExpr*)","signatureAssert":"Assertion failed: (inserted), function recordPackElementExpansion","signatureNext":"ConstraintGenerator::pushPackExpansionExpr"}
// RUN: not --crash %target-swift-frontend -typecheck %s
if case .a(repeat each b) = {} {
}
