// {"kind":"complete","original":"4be98c4c","signature":"swift::Parser::consumeDecl(swift::ParserPosition, bool)","signatureAssert":"Assertion failed: (!IDEInspectionDelayedDeclStat.get() && \"only one decl can be delayed for code completion\"), function setIDEInspectionDelayedDeclState"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
case /if { class a{ #^^#<#expression#>
