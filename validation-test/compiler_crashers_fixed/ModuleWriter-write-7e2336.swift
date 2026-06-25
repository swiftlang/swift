// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-path","/dev/null"],"kind":"emit-sil","original":"9385ecce","signature":"(anonymous namespace)::ModuleWriter::write()","signatureNext":"printModuleContentsAsCxx"}
// RUN: not %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-path /dev/null %s
extension a
  extension
