// RUN: %swift-frontend -target wasm32-unknown-wasip1 -parse-stdlib -module-name Swift -I %S/Inputs/custom-modules -typecheck %s

// REQUIRES: CODEGENERATOR=WebAssembly

import CVarArgs

/// ===== Minimal stdlib definitions =====
typealias Void = ()
struct CVaListPointer {}
// Optional definition is required for isOptional check in ClangImporter
enum Optional<T> {}
/// ======================================

func testVaList() {
  let _: (CVaListPointer) -> Void = CVarArgs.takeVaList
  let _: (CVaListPointer) -> Void = CVarArgs.takeVaList2
  let _: (CVaListPointer) -> Void = CVarArgs.takeVaList3
}
