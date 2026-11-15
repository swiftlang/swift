// {"extraArgs":["-enable-experimental-feature","CoroutineAccessors"],"kind":"emit-ir","signature":"swift::irgen::IRGenModule::createReplaceableProlog(swift::irgen::IRGenFunction&, swift::SILFunction*)","signatureNext":"IRGenModule::emitSILFunction"}
// RUN: not --crash %target-swift-frontend -emit-ir -enable-experimental-feature CoroutineAccessors %s
// REQUIRES: swift_feature_CoroutineAccessors
struct a<b> {
  var c: b
  dynamic subscript() -> b {
    read {
      yield
      c
    }
  }
}
