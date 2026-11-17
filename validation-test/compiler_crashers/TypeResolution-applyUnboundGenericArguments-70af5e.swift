// {"kind":"typecheck","original":"0edd32f5","signature":"swift::TypeResolution::applyUnboundGenericArguments(swift::GenericTypeDecl*, swift::Type, swift::SourceLoc, llvm::ArrayRef<swift::Type>, bool*) const","signatureNext":"ConstraintSystem::openUnboundGenericType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
\[<#type#>] = 0
