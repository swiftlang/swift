// {"kind":"typecheck","original":"35d466a5","signature":"swift::NominalTypeDecl::lookupConformance(swift::ProtocolDecl*, llvm::SmallVectorImpl<swift::ProtocolConformance*>&) const","signatureAssert":"Assertion failed: (!isa<ProtocolDecl>(this) && \"Self-conformances are only found by the higher-level \" \"swift::lookupConformance() entry point\"), function lookupConformance","signatureNext":"getIntroducedConformances"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Observation
@attached(extension conformances: Observable) macro a()
@a protocol b {
}
