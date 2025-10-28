// {"kind":"complete","original":"44c76fac","signature":"bool (anonymous namespace)::StmtChecker::typeCheckStmt<swift::Stmt>(swift::Stmt*&)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoContext"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a where b == <#type#>[ {
  func c() -> Self {
    #^^#
  }
}
