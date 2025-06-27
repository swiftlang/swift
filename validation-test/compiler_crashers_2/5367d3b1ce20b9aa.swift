// {"signature":"swift::NominalTypeDecl::lookupConformance(swift::ProtocolDecl*, llvm::SmallVectorImpl<swift::ProtocolConformance*>&) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
@objc protocol a{a} extension a {
  a class b : a
