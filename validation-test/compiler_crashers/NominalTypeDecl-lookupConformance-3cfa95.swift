// {"kind":"typecheck","original":"388a4c0b","signature":"swift::NominalTypeDecl::lookupConformance(swift::ProtocolDecl*, llvm::SmallVectorImpl<swift::ProtocolConformance*>&) const","signatureAssert":"Assertion failed: (!isa<ProtocolDecl>(this) && \"Self-conformances are only found by the higher-level \" \"swift::lookupConformance() entry point\"), function lookupConformance","signatureNext":"getIntroducedConformances"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@attached(extension conformances
: CustomStringConvertible) macro a()
@a protocol b {
}
