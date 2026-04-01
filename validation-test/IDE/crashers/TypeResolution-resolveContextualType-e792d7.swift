// {"kind":"complete","original":"30a31c51","signature":"swift::TypeResolution::resolveContextualType(swift::TypeRepr*, swift::DeclContext*, swift::GenericSignature, swift::TypeResolutionOptions, llvm::function_ref<swift::Type (swift::UnboundGenericType*)>, llvm::function_ref<swift::Type (swift::ASTContext&, swift::PlaceholderTypeRepr*)>, llvm::function_ref<swift::Type (swift::Type, swift::PackElementTypeRepr*)>, llvm::function_ref<void (swift::GenericTypeDecl*, llvm::function_ref<swift::Type (swift::SubstitutableType*)>)>, swift::SILTypeResolutionContext*)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment","signatureNext":"validateTypedPattern"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
macro a<b>() =
  {
    var c: b
    var <#pattern#>: #^^#
  }
