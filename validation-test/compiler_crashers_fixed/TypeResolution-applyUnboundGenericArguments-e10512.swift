// {"kind":"typecheck","original":"0f01932e","signature":"swift::Type llvm::function_ref<swift::Type (swift::SubstitutableType*)>::callback_fn<swift::TypeResolution::applyUnboundGenericArguments(swift::GenericTypeDecl*, swift::Type, swift::SourceLoc, llvm::ArrayRef<swift::Type>, bool*) const::$_0 const>(long, swift::SubstitutableType*)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->","signatureNext":"InFlightSubstitution::substType"}
// RUN: not %target-swift-frontend -typecheck %s
protocol a {
  extension a {
    struct b {
      struct c<d: c<d>> {
      }
    }
  }
}
