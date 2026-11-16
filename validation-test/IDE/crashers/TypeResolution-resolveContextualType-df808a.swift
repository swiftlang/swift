// {"kind":"complete","original":"7f37082c","signature":"swift::TypeResolution::resolveContextualType(swift::TypeRepr*, swift::DeclContext*, swift::GenericSignature, swift::TypeResolutionOptions, llvm::function_ref<swift::Type (swift::UnboundGenericType*)>, llvm::function_ref<swift::Type (swift::ASTContext&, swift::PlaceholderTypeRepr*)>, llvm::function_ref<swift::Type (swift::Type, swift::PackElementTypeRepr*)>, llvm::function_ref<void (swift::GenericTypeDecl*, llvm::function_ref<swift::Type (swift::SubstitutableType*)>)>, swift::SILTypeResolutionContext*)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment","signatureNext":"ExplicitCaughtTypeRequest::evaluate"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
enum a<b
  extension a: c  [ {
    func async {
      {
        async throws(b) -> d
        #^^#
