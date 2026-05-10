// {"kind":"emit-silgen","original":"072f7ff8","signature":"swift::Lowering::SILGenFunction::emitExistentialErasure(swift::SILLocation, swift::CanType, swift::Lowering::TypeLowering const&, swift::Lowering::TypeLowering const&, llvm::ArrayRef<swift::ProtocolConformanceRef>, swift::Lowering::SGFContext, llvm::function_ref<swift::Lowering::ManagedValue (swift::Lowering::SGFContext)>, bool)","signatureAssert":"Assertion failed: (metatype->getType().castTo<AnyMetatypeType>()->getRepresentation() == MetatypeRepresentation::Thick), function emitExistentialErasure","signatureNext":"RValueEmitter::visitErasureExpr"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a {
  associatedtype b
  static var c: b.Type {
    get
  }
}
struct d {
}
struct e: a {
  static var c: d.Type {
  }
}
func f() -> some a {
  e()
}
func g() -> Any.Type {
  let h = f()
  let i = type(of: h)
  return i.c
}
