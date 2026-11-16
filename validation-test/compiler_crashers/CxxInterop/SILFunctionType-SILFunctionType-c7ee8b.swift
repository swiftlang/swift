// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path"],"kind":"typecheck","original":"0ff3e4f1","signature":"swift::SILFunctionType::SILFunctionType(swift::GenericSignature, swift::SILExtInfo, swift::SILCoroutineKind, swift::ParameterConvention, llvm::ArrayRef<swift::SILParameterInfo>, llvm::ArrayRef<swift::SILYieldInfo>, llvm::ArrayRef<swift::SILResultInfo>, std::__1::optional<swift::SILResultInfo>, swift::SubstitutionMap, swift::SubstitutionMap, swift::ASTContext const&, swift::RecursiveTypeProperties, swift::ProtocolConformanceRef)","signatureAssert":"Assertion failed: (!param.hasOption(SILParameterInfo::NotDifferentiable) && \"non-`@differentiable` function type should not have \" \"`@noDerivative` parameter\"), function SILFunctionType","signatureNext":"SILFunctionType::get","splits":[0]}
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path %t/main.swift %t/x1.swift
//--- main.swift

//--- x1.swift
class a {
  var b: (@noDerivative Float) -> Float
}
