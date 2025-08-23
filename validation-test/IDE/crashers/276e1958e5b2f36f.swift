// {"kind":"complete","original":"14893775","signature":"swift::TypeResolution::resolveContextualType(swift::TypeRepr*, swift::DeclContext*, swift::GenericSignature, swift::TypeResolutionOptions, llvm::function_ref<swift::Type (swift::UnboundGenericType*)>, llvm::function_ref<swift::Type (swift::ASTContext&, swift::PlaceholderTypeRepr*)>, llvm::function_ref<swift::Type (swift::Type, swift::PackElementTypeRepr*)>, swift::SILTypeResolutionContext*)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoContext"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@a(
  #^^#) func b<a>()
