// {"kind":"complete","signature":"swift::Parser::parseLineDirective(bool)","signatureAssert":"Assertion failed: (isNewFile), function parseLineDirective"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{
#sourceLocation(file: "", line: 800)
#^COMPLETE^#
