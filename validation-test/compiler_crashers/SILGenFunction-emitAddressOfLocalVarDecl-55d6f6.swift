// {"kind":"emit-silgen","original":"52688747","signature":"swift::Lowering::SILGenFunction::emitAddressOfLocalVarDecl(swift::SILLocation, swift::VarDecl*, swift::CanType, swift::Lowering::SGFAccessKind)","signatureAssert":"Assertion failed: (address.isLValue()), function emitAddressOfLocalVarDecl","signatureNext":"SILGenLValue::visitPackElementExpr"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a {
  func b()
}
func c<each d: a>(f: repeat each d) async {
  await withThrowingTaskGroup { e in
    repeat e.addTask {
      (each f).b
    }
  }
}
