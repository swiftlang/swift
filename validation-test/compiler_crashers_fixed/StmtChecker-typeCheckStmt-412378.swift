// {"kind":"typecheck","signature":"bool (anonymous namespace)::StmtChecker::typeCheckStmt<swift::Stmt>(swift::Stmt*&)","signatureNext":"StmtChecker::typeCheckASTNode"}
// RUN: not %target-swift-frontend -typecheck %s
extension <#type#> {
  a {
  discard b
  }
