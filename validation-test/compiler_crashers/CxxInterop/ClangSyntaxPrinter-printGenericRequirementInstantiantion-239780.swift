// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"038ec259","signature":"swift::ClangSyntaxPrinter::printGenericRequirementInstantiantion(swift::GenericRequirement const&)","signatureAssert":"Assertion failed: (requirement.isAnyMetadata() && \"protocol requirements not supported yet!\"), function printGenericRequirementInstantiantion","signatureNext":"ClangSyntaxPrinter::printGenericRequirementsInstantiantions"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
struct a<each b> {
  var c: ()?
}
