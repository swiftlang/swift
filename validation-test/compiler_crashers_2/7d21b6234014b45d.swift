// {"signature":"swift::TypeResolution::applyUnboundGenericArguments(swift::GenericTypeDecl*, swift::Type, swift::SourceLoc, llvm::ArrayRef<swift::Type>) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a < let b : a<Int>> {
}
