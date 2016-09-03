// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-sil %s | %FileCheck %s

// REQUIRES: objc_interop

import ObjectiveC
import Foundation

class Foo {
  @objc(methodForInt:) func method(a: Int32) { }
  @objc(property) var isProperty: Bool = false
}

// CHECK-LABEL: sil hidden @_TF13objc_selector14createSelector
func createSelector(foo: Foo) -> Selector {
  // CHECK: [[LITERAL:%[0-9]+]] = string_literal objc_selector "methodForInt:"
  // CHECK-NEXT: [[PTR:%[0-9]+]] = struct $OpaquePointer ([[LITERAL]] : $Builtin.RawPointer)
  // CHECK-NEXT: [[SEL:%[0-9]+]] = struct $Selector (%3 : $OpaquePointer)
  // CHECK-: return [[SEL]] : $Selector
  return #selector(foo.method)
}

// CHECK-LABEL: sil hidden @{{.*}}createGetterSelector
func createGetterSelector() -> Selector {
  // CHECK: string_literal objc_selector "property"
  return #selector(getter: Foo.isProperty)
}

// CHECK-LABEL: sil hidden @{{.*}}createSetterSelector
func createSetterSelector() -> Selector {
  // CHECK: string_literal objc_selector "setProperty:"
  return #selector(setter: Foo.isProperty)
}
