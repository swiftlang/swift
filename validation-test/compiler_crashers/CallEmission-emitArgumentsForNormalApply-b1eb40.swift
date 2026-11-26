// {"frontendArgs":["-emit-silgen","-enable-experimental-concurrency","-enable-upcoming-feature","NonisolatedNonsendingByDefault"],"kind":"custom","signature":"(anonymous namespace)::CallEmission::emitArgumentsForNormalApply(swift::Lowering::AbstractionPattern, swift::CanTypeWrapper<swift::SILFunctionType>, llvm::ArrayRef<swift::LifetimeDependenceInfo>, swift::ForeignInfo const&, llvm::SmallVectorImpl<swift::Lowering::ManagedValue>&, std::__1::optional<swift::SILLocation>&)","signatureAssert":"Assertion failed: (SGF.ExpectedExecutor.isNecessary()), function emitArgumentsForNormalApply","signatureNext":"CallEmission::apply"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-experimental-concurrency -enable-upcoming-feature NonisolatedNonsendingByDefault %s
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault
func f(_: () async -> Void) reasync {
}
func g(_ x: () -> Void) {
  f(x)
}
