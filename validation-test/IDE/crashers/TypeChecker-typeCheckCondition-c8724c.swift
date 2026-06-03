// {"kind":"complete","original":"29246d54","signature":"swift::TypeChecker::typeCheckCondition(swift::Expr*&, swift::DeclContext*)","signatureAssert":"Assertion failed: (!expr->getType() || isa<OpaqueValueExpr>(expr) && \"the bool condition is already type checked\"), function typeCheckCondition","signatureNext":"StmtChecker::checkSiblingCaseStmts"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
switch <#expression#> {
case let a
where switch a {
case let b where 0:
  ::
  #^^#
}:
}
