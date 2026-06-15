// {"frontendArgs":["-emit-module","-o","/dev/null","-emit-symbol-graph","-emit-symbol-graph-dir","%t/out","-experimental-allow-module-with-compiler-errors"],"kind":"custom","signature":"swift::symbolgraphgen::SymbolGraphASTWalker::getRealModuleOf(swift::Decl const*) const","signatureNext":"SymbolGraphASTWalker::getModuleSymbolGraph"}
// RUN: %empty-directory(%t)
// RUN: not --crash %target-swift-frontend -emit-module -o /dev/null -emit-symbol-graph -emit-symbol-graph-dir %t/out -experimental-allow-module-with-compiler-errors %s
extension <#type#> {
}
