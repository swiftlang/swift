// {"kind":"typecheck","signature":"swift::TypeResolution::applyUnboundGenericArguments(swift::GenericTypeDecl*, swift::Type, swift::SourceLoc, llvm::ArrayRef<swift::Type>, bool*) const","signatureAssert":"Assertion failed: (genericSig->isConcreteType(gp)), function applyUnboundGenericArguments","signatureNext":"TypeResolver::applyGenericArguments"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  struct b<c> {
    typealias d<e> = a
  }
  extension b.d<Int> {
  }
}
