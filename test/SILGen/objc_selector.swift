// RUN: %target-swift-emit-sil -enable-sil-ownership -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -enable-objc-interop | %FileCheck %s

import ObjectiveC
import Foundation

class Foo {
  @objc(methodForInt:) func method(a: Int32) { }
  @objc(property) var isProperty: Bool = false
}

// CHECK-LABEL: sil hidden @$S13objc_selector14createSelector{{[_0-9a-zA-Z]*}}F
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
