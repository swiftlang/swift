// {"extraArgs":["-enable-experimental-feature","CoroutineAccessors"],"kind":"emit-silgen","signature":"swift::SILBuilder::createCopyValue(swift::SILLocation, swift::SILValue)","signatureAssert":"Assertion failed: (operand->getType().isObject()), function CopyValueInst","signatureNext":"LoadableAggTypeLowering::emitCopyValue"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-experimental-feature CoroutineAccessors %s
// REQUIRES: swift_feature_CoroutineAccessors
class a<b, c> {
  var d: (Int, b) {
    _read {
    }
  }
  func e(f: Int) -> b {
  }
}
class g: a<[g], g> {
  override func e(f: Int) -> [g] {
    d.1
  }
}
