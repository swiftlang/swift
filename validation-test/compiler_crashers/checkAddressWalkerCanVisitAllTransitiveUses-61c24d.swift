// {"frontendArgs":["-emit-silgen","-enable-experimental-com-interop","-enable-builtin-module","-module-name","COM","-language-mode","6"],"kind":"custom","signature":"(anonymous namespace)::checkAddressWalkerCanVisitAllTransitiveUses(swift::SILValue)","signatureNext":"SILVerifierBase::visitAllocStackInst"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-experimental-com-interop -enable-builtin-module -module-name COM -language-mode 6 %s
// REQUIRES: OS=macosx
import Builtin
public struct IID {
  init(
    data1: UInt32, data2: UInt16, data3: UInt16,
    data4: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  ) {
  }
}
@com(interface: "51000000-0000-0000-0000-000000000001")
protocol P {
}
func f(_ x: any P) -> Builtin.RawPointer {
  Builtin.bridgeToRawPointer(x)
}
