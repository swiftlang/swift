// {"extraArgs":["-enable-sil-opaque-values"],"kind":"emit-silgen","signature":"swift::Lowering::SILGenFunction::emitApply(std::__1::unique_ptr<swift::Lowering::ResultPlan, std::__1::default_delete<swift::Lowering::ResultPlan>>&&, swift::Lowering::ArgumentScope&&, swift::SILLocation, swift::Lowering::ManagedValue, swift::SubstitutionMap, llvm::ArrayRef<swift::Lowering::ManagedValue>, swift::Lowering::CalleeTypeInfo const&, swift::optionset::OptionSet<swift::ApplyFlags, unsigned char>, swift::Lowering::SGFContext, std::__1::optional<swift::ActorIsolation>)::$_0::operator()(swift::SILValue, swift::SILResultInfo const&) const","signatureAssert":"Assertion failed: (!substFnConv.isSILIndirect(resultInfo) && \"indirect direct result?\"), function operator()","signatureNext":"CallEmission::apply"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-sil-opaque-values %s
protocol a: Error {
}
func b<c: a>(_: c) -> c {
}
func d<e>(f: a) -> e {
  let g = b(f)
}
