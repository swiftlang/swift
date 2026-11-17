// {"kind":"emit-silgen","signature":"(anonymous namespace)::CallSite::emit(swift::Lowering::SILGenFunction&, swift::Lowering::AbstractionPattern, swift::CanTypeWrapper<swift::SILFunctionType>, llvm::ArrayRef<swift::LifetimeDependenceInfo>, (anonymous namespace)::ParamLowering&, llvm::SmallVectorImpl<swift::Lowering::ManagedValue>&, llvm::SmallVectorImpl<(anonymous namespace)::DelayedArgument>&, swift::ForeignInfo const&) &&","signatureAssert":"Assertion failed: (count <= Params.size()), function claimParams","signatureNext":"CallEmission::emitArgumentsForNormalApply"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
struct a<c> {
  var d: c
}
func e<each c>(b: a<(repeat each c) -> Int>, f: repeat each c) {
  b.d(repeat each f)
}
