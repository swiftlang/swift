// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-enable-library-evolution","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"233661fe","signature":"swift::LoweredFunctionSignature::visitParameterList(llvm::function_ref<void (swift::LoweredFunctionSignature::IndirectResultValue const&)>, llvm::function_ref<void (swift::LoweredFunctionSignature::DirectParameter const&)>, llvm::function_ref<void (swift::LoweredFunctionSignature::IndirectParameter const&)>, llvm::function_ref<void (swift::LoweredFunctionSignature::GenericRequirementParameter const&)>, llvm::function_ref<void (swift::LoweredFunctionSignature::MetadataSourceParameter const&)>, llvm::function_ref<void (swift::LoweredFunctionSignature::ContextParameter const&)>, llvm::function_ref<void (swift::LoweredFunctionSignature::ErrorResultValue const&)>) const","signatureAssert":"Assertion failed: (additionalParam), function operator()","signatureNext":"DeclAndTypeClangFunctionPrinter::printCxxThunkBody"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -enable-library-evolution -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
public
  enum a
{
  var b: a
}
