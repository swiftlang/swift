// {"frontendArgs":["-emit-silgen","-enable-experimental-com-interop","-module-name","COM"],"kind":"custom","signature":"swift::Lowering::SILGenFunction::emitExistentialErasure(swift::SILLocation, swift::CanType, swift::Lowering::TypeLowering const&, swift::Lowering::TypeLowering const&, llvm::ArrayRef<swift::ProtocolConformanceRef>, swift::Lowering::SGFContext, llvm::function_ref<swift::Lowering::ManagedValue (swift::Lowering::SGFContext)>, bool)","signatureNext":"RValueEmitter::visitErasureExpr"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-experimental-com-interop -module-name COM %s
struct IID {
  init(
    data1: UInt32, data2: UInt16, data3: UInt16,
    data4: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  ) {
  }
}
@com(interface: "10000000-0000-0000-0000-000000000001")
protocol P {
}
func f(_ x: some P) -> any P {
  x
}
