// {"kind":"typecheck","signature":"swift::TypeChecker::typeCheckStmtConditionElement(swift::StmtConditionElement&, bool&, swift::DeclContext*)","signatureAssert":"Assertion failed: (!elt.getPattern()->hasType() && \"the pattern binding condition is already type checked\"), function typeCheckPatternBindingStmtConditionElement"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a guard let b let a = b
