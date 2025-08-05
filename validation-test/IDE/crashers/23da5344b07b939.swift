// {"kind":"complete","signature":"swift::AnyFunctionType::getExtInfo() const","signatureAssert":"Assertion failed: (hasExtInfo()), function getExtInfo"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func a(b) a#^COMPLETE^#
