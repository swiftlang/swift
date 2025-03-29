// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Extern -emit-module-path %t/a.swiftmodule -module-name a %s
// RUN: llvm-bcanalyzer -dump %t/a.swiftmodule | %FileCheck --check-prefix BC-CHECK --implicit-check-not UnknownCode %s
// RUN: %target-swift-ide-test -print-module -module-to-print a -source-filename x -I %t | %FileCheck --check-prefix MODULE-CHECK %s

// REQUIRES: swift_feature_Extern

// BC-CHECK: <Extern_DECL_ATTR

// MODULE-CHECK: @_extern(wasm, module: "m0", name: "import1") func import1()
@_extern(wasm, module: "m0", name: "import1")
func import1()
