// {"extraArgs":["-enable-experimental-feature","BuiltinModule"],"kind":"emit-silgen","signature":"swift::SILBuilder::emitDestroyValueOperation(swift::SILLocation, swift::SILValue)","signatureNext":"Lowering::CleanupManager::emitCleanups"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-experimental-feature BuiltinModule %s
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_BuiltinModule
import Builtin
struct a {
  func b<c>(
    d: c.Type,
    f: Int
  ) {
    let e = f._builtinWordValue
    Builtin.allocWithTailElems_1(d, e, a.self)
  }
}
