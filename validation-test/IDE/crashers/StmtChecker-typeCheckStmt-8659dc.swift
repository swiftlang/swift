// {"kind":"complete","signature":"bool (anonymous namespace)::StmtChecker::typeCheckStmt<swift::Stmt>(swift::Stmt*&)","signatureNext":"StmtChecker::typeCheckASTNode"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension {
a { discard b#^COMPLETE^#
