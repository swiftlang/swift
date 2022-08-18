// RUN: %empty-directory(%t)
// RUN: %build-irgen-test-overlays

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: objc_interop

@_weakLinked import Foundation

func testObjCClass() {
  // CHECK-DAG: @"OBJC_CLASS_$_NSNotification" = extern_weak global %objc_class
  _ = NSNotification()
}

func testGlobalVariables() {
  // CHECK-DAG: @weak_variable = extern_weak global
  _ = weak_variable

  // CHECK-DAG: @strong_variable = extern_weak global
  _ = strong_variable
}

func testFunctions() {
  // CHECK-DAG: declare extern_weak void @always_available_function()
  always_available_function()
}

// CHECK-DAG: @"OBJC_CLASS_$_NSNumber" = extern_weak global %objc_class
// CHECK-DAG: @"OBJC_METACLASS_$_NSNumber" = extern_weak global %objc_class
class CustomNumber: NSNumber {}
