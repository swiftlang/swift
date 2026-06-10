// {"kind":"complete","original":"0bdbc8ea","signature":"swift::Parser::parseExprSelector()","signatureAssert":"Assertion failed: (getPtrOrNull() && \"not checked for nullptr\"), function get","signatureNext":"Parser::parseExprUnary"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
#selector(a throws  #^^#
