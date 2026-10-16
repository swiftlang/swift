// {"kind":"complete","original":"8d2c6e37","signature":"(anonymous namespace)::TypeResolver::resolveTupleType(swift::TupleTypeRepr*, swift::TypeResolutionOptions)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment","signatureNext":"TypeResolver::resolveType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
macro a<b>(c: (b, String) #^^#
