// {"kind":"complete","original":"bcb7c8c1","signature":"(anonymous namespace)::ConstraintGenerator::visitTapExpr(swift::TapExpr*)","signatureAssert":"Assertion failed: ((varDC == CS.DC || isa<AbstractClosureExpr>(varDC) || varDC->isChildContextOf(CS.DC)) && \"TapExpr var should be in the same DeclContext we're checking it in!\"), function visitTapExpr"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func a -> @b("\()." #^^#
