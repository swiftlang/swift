// {"extraArgs":["-experimental-allow-module-with-compiler-errors"],"kind":"emit-sil","original":"02a7072e","signature":"(anonymous namespace)::TypeResolver::diagnoseGenericArgumentsOnSelf(swift::UnqualifiedIdentTypeRepr*, swift::DeclContext*)","signatureNext":"TypeResolver::resolveDeclRefTypeReprRec"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors %s
extension <#type#> {
  typealias a = Self<>
}
