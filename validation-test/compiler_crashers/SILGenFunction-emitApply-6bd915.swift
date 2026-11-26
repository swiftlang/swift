// {"frontendArgs":["-emit-silgen","-enable-experimental-concurrency","-language-mode","6"],"kind":"custom","signature":"swift::Lowering::SILGenFunction::emitApply(std::__1::unique_ptr<swift::Lowering::ResultPlan, std::__1::default_delete<swift::Lowering::ResultPlan>>&&, swift::Lowering::ArgumentScope&&, swift::SILLocation, swift::Lowering::ManagedValue, swift::SubstitutionMap, llvm::ArrayRef<swift::Lowering::ManagedValue>, swift::Lowering::CalleeTypeInfo const&, swift::optionset::OptionSet<swift::ApplyFlags, unsigned char>, swift::Lowering::SGFContext, std::__1::optional<swift::ActorIsolation>)","signatureAssert":"Assertion failed: (F.isAsync()), function emitApply","signatureNext":"CallEmission::apply"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-experimental-concurrency -language-mode 6 %s
func f(_ x: @autoclosure () async -> Bool) reasync {
  _ = await x()
}
@MainActor func g() {
  withExtendedLifetime(0) {
    f(true)
  }
}
