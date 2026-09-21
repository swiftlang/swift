// {"kind":"typecheck","signature":"swift::TypeResolution::applyUnboundGenericArguments(swift::GenericTypeDecl*, swift::Type, swift::SourceLoc, llvm::ArrayRef<swift::Type>, bool*) const","signatureAssert":"Assertion failed: (genericSig->isConcreteType(gp)), function applyUnboundGenericArguments","signatureNext":"TypeResolver::applyGenericArguments"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b> {
  typealias c<d> = Int
}
func e(): a.c<Int>
