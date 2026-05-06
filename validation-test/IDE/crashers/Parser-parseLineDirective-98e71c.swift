// {"kind":"complete","original":"09355d88","signature":"swift::Parser::parseLineDirective(bool)","signatureAssert":"Assertion failed: (isNewFile), function parseLineDirective","signatureNext":"Parser::parseDeclItem"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
enum a: #^^# {
  #sourceLocation(file:"",line:1)
}
