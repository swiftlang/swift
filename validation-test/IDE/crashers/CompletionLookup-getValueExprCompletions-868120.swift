// {"kind":"complete","signature":"swift::ide::CompletionLookup::getValueExprCompletions(swift::Type, swift::ValueDecl*, bool)","signatureAssert":"Assertion failed: (IsUnwrappedOptional && \"IUOs should be optional if not bound/forced\"), function tryUnwrappedCompletions"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func a(b: (() -> Float)!) {
b()#^COMPLETE^#
