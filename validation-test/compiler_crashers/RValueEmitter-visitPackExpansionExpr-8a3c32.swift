// {"kind":"emit-silgen","original":"2b600598","signature":"void llvm::function_ref<void (swift::Lowering::Initialization*)>::callback_fn<(anonymous namespace)::RValueEmitter::visitPackExpansionExpr(swift::PackExpansionExpr*, swift::Lowering::SGFContext)::$_0::operator()(swift::SILValue, swift::SILValue, swift::SILValue) const::'lambda'(swift::Lowering::Initialization*)>(long, swift::Lowering::Initialization*)","signatureAssert":"Assertion failed: (!E->getType()->hasLValueType() && \"l-values must be emitted with emitLValue\"), function emitRValue","signatureNext":"Lowering::InPlacePackExpansionInitialization::performPackExpansionInitialization"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
class a<b> {
  var c: b
  init(_: b) {
  }
}
func d<each e>(args: repeat each e, f: Bool) -> () -> (repeat each e) {
  let g = (repeat a(each args))
  if f {
    return {
      (repeat (each g).c)
    }
  }
}
