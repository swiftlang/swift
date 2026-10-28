// {"kind":"typecheck","signature":"swift::TypeResolution::applyUnboundGenericArguments(swift::GenericTypeDecl*, swift::Type, swift::SourceLoc, llvm::ArrayRef<swift::Type>, bool*) const","signatureAssert":"Assertion failed: (genericSig->isConcreteType(gp)), function applyUnboundGenericArguments","signatureNext":"TypeResolver::applyGenericArguments"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < b {
struct c < b{typealias d<e> = a} func f : c.d<Int>
