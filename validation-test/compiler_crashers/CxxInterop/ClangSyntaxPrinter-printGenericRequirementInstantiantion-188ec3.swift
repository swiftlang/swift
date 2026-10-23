// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"0439ba2e","signature":"swift::ClangSyntaxPrinter::printGenericRequirementInstantiantion(swift::GenericRequirement const&)","signatureAssert":"Assertion failed: (requirement.isAnyMetadata() && \"protocol requirements not supported yet!\"), function printGenericRequirementInstantiantion","signatureNext":"ClangSyntaxPrinter::printGenericRequirementsInstantiantions"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
protocol a {
  associatedtype b
}
@propertyWrapper struct f<each c> {
  var wrappedValue: (repeat each c)
}
struct d<each e: a> {
  @f var g: (repeat each e.b)
}
