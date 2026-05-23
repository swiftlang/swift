// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"23a03460","signature":"swift::NormalProtocolConformance::getAssociatedConformance(swift::Type, swift::ProtocolDecl*) const","signatureAssert":"Assertion failed: (!AssociatedConformances[index]), function setAssociatedConformance","signatureNext":"Serializer::writeLocalNormalProtocolConformance"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
protocol a {
  associatedtype b: c where b.d == Self
}
protocol c {
  associatedtype d: F
}
protocol F {
  associatedtype b
  struct e: a {
  }
}
