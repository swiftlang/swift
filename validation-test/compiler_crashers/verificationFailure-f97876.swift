// {"extraArgs":["-enable-experimental-feature","Embedded"],"kind":"emit-silgen","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifierBase::visitClassMethodInst"}
// RUN: not --crash %target-swift-frontend -target %target-cpu-apple-macos14 -emit-silgen -enable-experimental-feature Embedded %s
// REQUIRES: swift_feature_Embedded
// REQUIRES: OS=macosx
protocol a {
  func b<c>(d: c) -> c
}
class e: a {
  func b<c>(d: c) -> c {
  }
}
