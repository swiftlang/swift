// {"kind":"emit-silgen","signature":"swift::Lowering::SILGenFunction::emitAddressOfLValue(swift::SILLocation, swift::Lowering::LValue&&, swift::Lowering::TSanKind)","signatureAssert":"Assertion failed: (addr.getType().isAddress() && \"resolving lvalue did not give an address\"), function emitAddressOfLValue","signatureNext":"SILGenLValue::visitBindOptionalExpr"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s

struct S {
  var p: UnsafeMutablePointer<Int?>
  var f: Int? {
    @_unsafeSelfDependentResult borrow { p.pointee }
    mutate { &p.pointee }
  }
  mutating func g() {
    _ = f?.magnitude
  }
}
