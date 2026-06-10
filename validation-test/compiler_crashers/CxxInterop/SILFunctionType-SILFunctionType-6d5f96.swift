// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"c5806155","signature":"swift::SILFunctionType::SILFunctionType(swift::GenericSignature, swift::SILExtInfo, swift::SILCoroutineKind, swift::ParameterConvention, llvm::ArrayRef<swift::SILParameterInfo>, llvm::ArrayRef<swift::SILYieldInfo>, llvm::ArrayRef<swift::SILResultInfo>, std::__1::optional<swift::SILResultInfo>, swift::SubstitutionMap, swift::SubstitutionMap, swift::ASTContext const&, swift::RecursiveTypeProperties, swift::ProtocolConformanceRef)","signatureAssert":"Assertion failed: (!result.getInterfaceType()->hasError() && \"interface type of result should not contain error types\"), function SILFunctionType","signatureNext":"SILFunctionType::get"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
struct a<b {
  d: b
  protocol e {
    associatedtype f: Collection
    func l -> f
  }
  struct g: e {
    h: Range<Int>
    func i<j: e>(_ c: j) -> some Collection {
      c.l(
    }
    k -> some Collection {
    i(g(h: 0..<0
