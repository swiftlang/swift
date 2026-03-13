// {"kind":"complete","original":"00fc7609","signature":"(anonymous namespace)::CodeCompletionCallbacksImpl::readyForTypeChecking(swift::SourceFile*)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a:Self#^^#
