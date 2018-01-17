// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -primary-file %s -import-objc-header %S/Inputs/objc_bridged_generic_conformance.h | %FileCheck %s
// REQUIRES: objc_interop

// CHECK-NOT: _TMnCSo

// CHECK: @"\01l_protocol_conformances" = {{.*}} @"got.OBJC_CLASS_$_Thingy"

// CHECK-NOT: _TMnCSo

protocol P { func test() }

extension Thingy: P {
  func test() {}
}
