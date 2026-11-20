// {"extraArgs":["-enable-experimental-feature","BuiltinModule"],"kind":"emit-silgen","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifierBase::visitEndLifetimeInst"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-experimental-feature BuiltinModule %s
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_BuiltinModule
import Builtin
func a<b>(c: b) {
  do {
    let d: Int = try Builtin.emplace { e throws in
    }
  } catch {
  }
}
