// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"03a34d91","signature":"void llvm::function_ref<void (swift::GenericRequirement)>::callback_fn<(anonymous namespace)::PolymorphicConvention::enumerateUnfulfilledRequirements(llvm::function_ref<void (swift::GenericRequirement)> const&)::$_0>(long, swift::GenericRequirement)","signatureAssert":"Assertion failed: (generics->isReducedType(reqt.getTypeParameter())), function operator()","signatureNext":"enumerateGenericSignatureRequirements"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
protocol a {
  associatedtype b
  func c -> b
}
struct d<e>: a {
  f: e
  func c -> e
}
protocol i where g == Self
  extension d where b: i {
    struct h {  f: Int
