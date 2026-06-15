// {"kind":"complete","original":"3288ce72","signature":"swift::TypeResolution::applyUnboundGenericArguments(swift::GenericTypeDecl*, swift::Type, swift::SourceLoc, llvm::ArrayRef<swift::Type>, bool*) const","signatureAssert":"Assertion failed: (genericSig->isConcreteType(gp)), function applyUnboundGenericArguments","signatureNext":"TypeResolver::applyGenericArguments"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<b> {
  typealias c<d> = <#type#>
}
let <#pattern#>: a.c<String> = #^^#
