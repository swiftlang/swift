// {"kind":"emit-silgen","signature":"SILGenLValue::visitBindOptionalExpr(swift::BindOptionalExpr*, swift::Lowering::SGFAccessKind, swift::Lowering::LValueOptions)","signatureAssert":"Assertion failed: (addr.getType().isAddress() && \"resolving lvalue did not give an address\"), function emitAddressOfLValue","signatureNext":"Lowering::SILGenFunction::emitLValue"}
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
