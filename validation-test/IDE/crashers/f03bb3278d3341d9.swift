// {"kind":"complete","signature":"swift::Parser::consumeTopLevelDecl(swift::ParserPosition, swift::TopLevelCodeDecl*)","signatureAssert":"Assertion failed: (!IDEInspectionDelayedDeclStat.get() && \"only one decl can be delayed for code completion\"), function setIDEInspectionDelayedDeclState"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
guard {
#if #^COMPLETE^#
0
