// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"emit-sil","original":"aea5bba9","signature":"void llvm::function_ref<void (swift::LoweredFunctionSignature::IndirectParameter const&)>::callback_fn<swift::DeclAndTypeClangFunctionPrinter::printCxxThunkBody(swift::AbstractFunctionDecl const*, swift::LoweredFunctionSignature const&, llvm::StringRef, swift::NominalTypeDecl const*, swift::ModuleDecl const*, swift::Type, swift::ParameterList const*, bool, swift::AnyFunctionType const*, bool, std::__1::optional<swift::IRABIDetailsProvider::MethodDispatchInfo>)::$_9::operator()(std::__1::optional<llvm::StringRef>) const::'lambda'(swift::LoweredFunctionSignature::IndirectParameter const&)>(long, swift::LoweredFunctionSignature::IndirectParameter const&)","signatureAssert":"Assertion failed: (!result.isUnsupported()), function operator()","signatureNext":"LoweredFunctionSignature::visitParameterList"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
class a<b> {
  class func  0()
  .
}
class c: a {
}
