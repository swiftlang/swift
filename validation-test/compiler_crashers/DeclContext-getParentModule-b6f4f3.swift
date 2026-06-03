// {"kind":"typecheck","original":"bfba1ecf","signature":"swift::DeclContext::getParentModule() const","signatureNext":"Decl::isInMacroExpansionInContext"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@attached(accessor) macro a()
@a("\()") var b
