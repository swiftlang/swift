// {"kind":"emit-silgen","signature":"swift::Lowering::RValue::RValue(swift::Lowering::SILGenFunction*, llvm::ArrayRef<swift::Lowering::ManagedValue>, swift::CanType)","signatureAssert":"Assertion failed: (values.size() == expectedExplosionSize(type) && \"creating rvalue with wrong number of pre-exploded elements\"), function RValue","signatureNext":"RValueEmitter::visitConsumeExpr"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
func a<b>() -> b {
  var c: ()
  consume c
}
