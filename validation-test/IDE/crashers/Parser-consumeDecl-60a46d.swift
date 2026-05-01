// {"kind":"complete","original":"f9c166f3","signature":"swift::Parser::consumeDecl(swift::ParserPosition, bool)","signatureAssert":"Assertion failed: (!IDEInspectionDelayedDeclStat.get() && \"only one decl can be delayed for code completion\"), function setIDEInspectionDelayedDeclState","signatureNext":"Parser::parseBraceItems"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
let a =
  if <#expression#> {
    #if #^^#
      {
      }
    #endif
  }
