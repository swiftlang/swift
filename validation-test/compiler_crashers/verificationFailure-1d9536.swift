// {"extraArgs":["-disable-availability-checking"],"kind":"emit-silgen","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifier::checkFunctionRefBaseInst"}
// RUN: not --crash %target-swift-frontend -emit-silgen -disable-availability-checking %s
struct a {
}
public func b<c>(d: c = a()) {
}
