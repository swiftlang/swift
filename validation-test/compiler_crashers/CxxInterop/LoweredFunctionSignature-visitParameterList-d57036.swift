// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"a439ee77","signature":"swift::LoweredFunctionSignature::visitParameterList(llvm::function_ref<void (swift::LoweredFunctionSignature::IndirectResultValue const&)>, llvm::function_ref<void (swift::LoweredFunctionSignature::DirectParameter const&)>, llvm::function_ref<void (swift::LoweredFunctionSignature::IndirectParameter const&)>, llvm::function_ref<void (swift::LoweredFunctionSignature::GenericRequirementParameter const&)>, llvm::function_ref<void (swift::LoweredFunctionSignature::MetadataSourceParameter const&)>, llvm::function_ref<void (swift::LoweredFunctionSignature::ContextParameter const&)>, llvm::function_ref<void (swift::LoweredFunctionSignature::ErrorResultValue const&)>) const","signatureAssert":"Assertion failed: (!result.isUnsupported()), function operator()","signatureNext":"DeclAndTypeClangFunctionPrinter::printCxxThunkBody"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
struct a
  protocol b
    struct c<d {
      e: [a]
      struct f<g { i : [a]mutating func h where g == b
