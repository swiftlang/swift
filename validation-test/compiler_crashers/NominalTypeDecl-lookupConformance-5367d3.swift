// {"kind":"typecheck","signature":"swift::NominalTypeDecl::lookupConformance(swift::ProtocolDecl*, llvm::SmallVectorImpl<swift::ProtocolConformance*>&) const","signatureAssert":"Assertion failed: (!isa<ProtocolDecl>(this) && \"Self-conformances are only found by the higher-level \" \"swift::lookupConformance() entry point\"), function lookupConformance"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
@objc protocol a{a} extension a {
  a class b : a
