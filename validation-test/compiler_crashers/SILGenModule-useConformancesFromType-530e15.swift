// {"extraArgs":["-language-mode","6"],"kind":"emit-silgen","signature":"void llvm::function_ref<void (swift::CanType)>::callback_fn<swift::Lowering::SILGenModule::useConformancesFromType(swift::SILInstruction*, swift::CanType)::$_0>(long, swift::CanType)","signatureNext":"CanType::visit"}
// RUN: not --crash %target-swift-frontend -emit-silgen -language-mode 6 %s
// rdar://192004206
struct a: ~Copyable {
  var b: Int
}
_ = \a.b
