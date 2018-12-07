// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -primary-file %s -enable-objc-interop -import-objc-header %S/Inputs/objc_bridged_generic_conformance.h | %FileCheck %s

// CHECK-NOT: _TMnCSo

// CHECK: @"$sSo6ThingyCyxG32objc_bridged_generic_conformance1PADMc" = hidden constant %swift.protocol_conformance_descriptor {{.*}} @[[THINGY_NAME:[0-9]]]

// CHECK: @[[THINGY_NAME]] = private constant [7 x i8] c"Thingy\00"

// CHECK-NOT: _TMnCSo

protocol P { func test() }

extension Thingy: P {
  @objc func test() {}
}
