// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"emit-sil","original":"6e7f0216","signature":"overwriteForwardDecl(llvm::DenseMap<swift::TypeBase*, swift::irgen::TypeInfo const*, llvm::DenseMapInfo<swift::TypeBase*, void>, llvm::detail::DenseMapPair<swift::TypeBase*, swift::irgen::TypeInfo const*>>&, swift::TypeBase*, swift::irgen::TypeInfo const*)","signatureAssert":"Assertion failed: (cache.count(key) && \"no forward declaration?\"), function overwriteForwardDecl","signatureNext":"TypeConverter::convertAnyNominalType"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
struct a<b> {
  var c: a<String>
}
