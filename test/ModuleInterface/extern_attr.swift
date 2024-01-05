// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -enable-experimental-feature Extern -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -enable-experimental-feature Extern -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s < %t/Library.swiftinterface

// CHECK:      #if compiler(>=5.3) && $Extern
// CHECK-NEXT:   @_extern(c) public func externalCFunc()
// CHECK-NEXT: #endif
@_extern(c) public func externalCFunc()

// CHECK:      #if compiler(>=5.3) && $Extern
// CHECK-NEXT:   @_extern(c, "renamedCFunc") public func externalRenamedCFunc()
// CHECK-NEXT: #endif
@_extern(c, "renamedCFunc") public func externalRenamedCFunc()

// CHECK:      #if compiler(>=5.3) && $Extern
// CHECK-NEXT:   @_extern(wasm, module: "m", name: "f") public func wasmImportedFunc()
// CHECK-NEXT: #endif
@_extern(wasm, module: "m", name: "f") public func wasmImportedFunc()
