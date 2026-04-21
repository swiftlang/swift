// {"kind":"emit-silgen","original":"07205968","signature":"void llvm::function_ref<void (swift::Lowering::Initialization*)>::callback_fn<(anonymous namespace)::RValueEmitter::visitPackExpansionExpr(swift::PackExpansionExpr*, swift::Lowering::SGFContext)::$_0::operator()(swift::SILValue, swift::SILValue, swift::SILValue) const::'lambda'(swift::Lowering::Initialization*)>(long, swift::Lowering::Initialization*)","signatureAssert":"Assertion failed: (!E->getType()->hasLValueType() && \"l-values must be emitted with emitLValue\"), function emitRValue","signatureNext":"Lowering::BlackHoleInitialization::performPackExpansionInitialization"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a: AnyObject {
  var b: Double {
    get
    set
  }
}
func c<each d: a>(e: repeat each d) {
  let f = (repeat each e)
  let _ = (repeat (each f).b)
}
