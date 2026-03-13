// {"kind":"complete","original":"3521466b","signature":"(anonymous namespace)::TypeResolver::resolvePackExpansionType(swift::PackExpansionTypeRepr*, swift::TypeResolutionOptions)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null GenericSignature!\"), function operator->"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
macro a<each b>(c: (repeat each b) #^^#
