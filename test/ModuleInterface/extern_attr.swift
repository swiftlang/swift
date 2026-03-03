// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -enable-experimental-feature Extern -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -enable-experimental-feature Extern -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s < %t/Library.swiftinterface

// REQUIRES: swift_feature_Extern

// CHECK:   @_extern(c) public func externalCFunc()
@_extern(c) public func externalCFunc()

// CHECK:   @_extern(c, "renamedCFunc") public func externalRenamedCFunc()
@_extern(c, "renamedCFunc") public func externalRenamedCFunc()

// CHECK:   @_extern(wasm, module: "m", name: "f") public func wasmImportedFunc()
@_extern(wasm, module: "m", name: "f") public func wasmImportedFunc()
