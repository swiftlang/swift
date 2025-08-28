// {"kind":"complete","original":"0057cadb","signature":"(anonymous namespace)::CodeCompletionCallbacksImpl::typecheckParsedType()","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoContext"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{class a<b where b#^^#
