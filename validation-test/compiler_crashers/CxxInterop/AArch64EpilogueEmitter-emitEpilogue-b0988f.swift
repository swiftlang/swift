// {"frontendArgs":["-emit-object","-o","/dev/null","-cxx-interoperability-mode=default","-enable-experimental-feature","CoroutineAccessors","-enable-arm64-corocc"],"kind":"custom","signature":"llvm::AArch64EpilogueEmitter::emitEpilogue()","signatureAssert":"Assertion failed: (!EmitCFI), function emitEpilogue","signatureNext":"AArch64FrameLowering::emitEpilogue"}
// RUN: not --crash %target-swift-frontend -emit-object -o /dev/null -cxx-interoperability-mode=default -enable-experimental-feature CoroutineAccessors -enable-arm64-corocc -I %S/Inputs/AArch64EpilogueEmitter-emitEpilogue-8153bc %s
// REQUIRES: CPU=arm64 || CPU=arm64e
// REQUIRES: swift_feature_CoroutineAccessors

import CxxMod

public struct Box {
  var value: Int32 = 0
  public var v: Int32 {
    get { value }
    yielding mutate { mayThrow(); yield &value }
  }
}
