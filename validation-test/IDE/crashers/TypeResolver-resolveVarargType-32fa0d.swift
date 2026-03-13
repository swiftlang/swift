// {"kind":"complete","original":"95ab8e45","signature":"(anonymous namespace)::TypeResolver::resolveVarargType(swift::VarargTypeRepr*, swift::TypeResolutionOptions)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<b
  protocol c: a<Self
... >#^^#
