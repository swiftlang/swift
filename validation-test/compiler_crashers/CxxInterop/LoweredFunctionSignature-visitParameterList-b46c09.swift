// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"5748b576","signature":"swift::LoweredFunctionSignature::visitParameterList(llvm::function_ref<void (swift::LoweredFunctionSignature::IndirectResultValue const&)>, llvm::function_ref<void (swift::LoweredFunctionSignature::DirectParameter const&)>, llvm::function_ref<void (swift::LoweredFunctionSignature::IndirectParameter const&)>, llvm::function_ref<void (swift::LoweredFunctionSignature::GenericRequirementParameter const&)>, llvm::function_ref<void (swift::LoweredFunctionSignature::MetadataSourceParameter const&)>, llvm::function_ref<void (swift::LoweredFunctionSignature::ContextParameter const&)>, llvm::function_ref<void (swift::LoweredFunctionSignature::ErrorResultValue const&)>) const","signatureNext":"IRABIDetailsProviderImpl::getFunctionLoweredSignature"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-path /dev/null %s
public
  class a
{
  @_silgen_name("x")public <#declaration#>
  b
}
@_silgen_name("x") public func c()
