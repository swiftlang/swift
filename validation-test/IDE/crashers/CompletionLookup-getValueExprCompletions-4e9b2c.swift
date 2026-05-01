// {"kind":"complete","original":"066eb6bc","signature":"swift::ide::CompletionLookup::getValueExprCompletions(swift::Type, swift::ValueDecl*, bool)","signatureAssert":"Assertion failed: (!ExprType->hasTypeParameter()), function getValueExprCompletions","signatureNext":"PostfixCompletionCallback::collectResults"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<b{ c<let d: b{ d #^^#
