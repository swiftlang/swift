// {"kind":"complete","signature":"bool (anonymous namespace)::StmtChecker::typeCheckStmt<swift::Stmt>(swift::Stmt*&)","signatureAssert":"Assertion failed: (size() >= N && \"Dropping more elements than exist\"), function drop_front"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
let:{ let a=
#^COMPLETE^#{
b:{
