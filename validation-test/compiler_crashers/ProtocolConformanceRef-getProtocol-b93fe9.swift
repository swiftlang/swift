// {"extraArgs":["-enable-experimental-feature","KeyPathWithMethodMembers"],"kind":"emit-silgen","signature":"swift::ProtocolConformanceRef::getProtocol() const","signatureAssert":"Assertion failed: (isAbstract()), function getAbstract","signatureNext":"WitnessMethodInst::create"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-experimental-feature KeyPathWithMethodMembers %s
// REQUIRES: swift_feature_KeyPathWithMethodMembers
protocol a {
  associatedtype b
  associatedtype c
  func d(e: b) -> c
}
extension a {
  var f: KeyPath<Self, (b) -> c> {
    \.d
  }
}
