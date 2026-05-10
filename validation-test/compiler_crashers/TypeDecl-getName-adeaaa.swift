// {"extraArgs":["-experimental-allow-module-with-compiler-errors"],"kind":"emit-sil","original":"2a255fd2","signature":"swift::TypeDecl::getName() const","signatureAssert":"Assertion failed: (!isa<SendableAttr>(attr) && \"Conformance should have been added by SynthesizedProtocolAttr!\"), function deriveImplicitSendableConformance"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors %s
extension {
  @Sendable struct a
