// {"kind":"typecheck","original":"29abd1cd","signature":"swift::TypeResolution::resolveContextualType(swift::TypeRepr*, swift::DeclContext*, swift::GenericSignature, swift::TypeResolutionOptions, llvm::function_ref<swift::Type (swift::UnboundGenericType*)>, llvm::function_ref<swift::Type (swift::ASTContext&, swift::PlaceholderTypeRepr*)>, llvm::function_ref<swift::Type (swift::Type, swift::PackElementTypeRepr*)>, llvm::function_ref<void (swift::GenericTypeDecl*, llvm::function_ref<swift::Type (swift::SubstitutableType*)>)>, swift::SILTypeResolutionContext*)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment","signatureNext":"ConstraintGenerator::addSpecializationConstraint"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@attached(member) macro a()
@a<c> struct b<c> {
}
