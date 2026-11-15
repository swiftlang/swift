// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"79f00eed","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILFunction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILFunction::verify"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-path /dev/null %s
// REQUIRES: objc_interop
@objc public func a()
