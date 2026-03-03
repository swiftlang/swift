// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend  -module-name attr_expose -emit-module -parse-as-library -o %t %s
// RUN: %target-sil-opt -enable-sil-verify-all %t/attr_expose.swiftmodule | %FileCheck %s

// @_expose
// -----------------------------------------------------------------------------

// CHECK:      @_expose(Cxx)
// CHECK-NEXT: func exposeToCxx()
@_expose(Cxx)
func exposeToCxx() -> Int { return 42 }

// CHECK:      @_expose(Cxx, "custom_name")
// CHECK-NEXT: func exposeToCxxWithName()
@_expose(Cxx, "custom_name")
func exposeToCxxWithName() -> Int { return 24 }

// CHECK:      @_expose(!Cxx)
// CHECK-NEXT: func dontExposeToCxx()
@_expose(!Cxx)
func dontExposeToCxx() -> Int { return 13 }

// CHECK:      @_expose(wasm)
// CHECK-NEXT: func exposeToWasm()
@_expose(wasm)
func exposeToWasm() -> Int { return 99 }

// CHECK:     @_expose(wasm, "wasm_custom")
// CHECK-NEXT func exposeToWasmWithName()
@_expose(wasm, "wasm_custom")
func exposeToWasmWithName() -> Int { return 88 }
