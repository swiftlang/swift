// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -enable-experimental-feature Extern -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -enable-experimental-feature Extern -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s < %t/Library.swiftinterface

// CHECK:      #if compiler(>=5.3) && $Extern
// CHECK-NEXT:   @extern(c) public func externalCFunc()
// CHECK-NEXT: #endif
@extern(c) public func externalCFunc()

// CHECK:      #if compiler(>=5.3) && $Extern
// CHECK-NEXT:   @extern(c, "renamedCFunc") public func externalRenamedCFunc()
// CHECK-NEXT: #endif
@extern(c, "renamedCFunc") public func externalRenamedCFunc()

// CHECK:      #if compiler(>=5.3) && $Extern
// CHECK-NEXT:   @extern(wasm, module: "m", name: "f") public func wasmImportedFunc()
// CHECK-NEXT: #endif
@extern(wasm, module: "m", name: "f") public func wasmImportedFunc()
