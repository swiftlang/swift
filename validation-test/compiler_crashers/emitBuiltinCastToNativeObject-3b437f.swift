// {"extraArgs":["-enable-experimental-feature","BuiltinModule"],"kind":"emit-silgen","signature":"emitBuiltinCastToNativeObject(swift::Lowering::SILGenFunction&, swift::SILLocation, swift::SubstitutionMap, llvm::ArrayRef<swift::Lowering::ManagedValue>, swift::Lowering::SGFContext)","signatureAssert":"Assertion failed: (ty->getReferenceCounting() == ReferenceCounting::Native && \"Can only cast types that use native reference counting to native \" \"object\"), function emitBuiltinCastToNativeObject","signatureNext":"CallEmission::apply"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-experimental-feature BuiltinModule %s
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_BuiltinModule
import Builtin
protocol a {
}
func b(
  c: a
) {
  Builtin.castToNativeObject(c)
}
