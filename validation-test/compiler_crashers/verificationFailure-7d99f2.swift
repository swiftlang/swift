// {"extraArgs":["-enable-experimental-feature","BuiltinModule"],"kind":"emit-silgen","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifierBase::visitRefTailAddrInst"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-experimental-feature BuiltinModule %s
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_BuiltinModule
import Builtin
class a<b> {
  func c<g: a>(
    d: g.Type,
    h: Int
  ) {
    let e = h._builtinWordValue
    let f = Builtin.allocWithTailElems_1(d, e, b.self)
    Builtin.projectTailElems(f, b.self)
  }
}
