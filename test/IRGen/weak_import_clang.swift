// RUN: %empty-directory(%t)
// RUN: %build-irgen-test-overlays
//
// Specify explicit target triples for the deployment target to test weak
// linking for a symbol introduced in OS X 10.51.
//
// RUN: %target-swift-frontend(mock-sdk: -target %target-cpu-apple-macosx10.50 -sdk %S/Inputs -I %t) -primary-file %s -emit-ir | %FileCheck -check-prefix=CHECK-10_50 %s
// RUN: %target-swift-frontend(mock-sdk: -target %target-cpu-apple-macosx10.51 -sdk %S/Inputs -I %t) -primary-file %s -emit-ir | %FileCheck -check-prefix=CHECK-10_51 %s

// REQUIRES: OS=macosx
// REQUIRES: objc_interop

// FIXME: This test in written in Swift because the SIL parser fails
// when referencing weak_variable.

import Foundation

// CHECK-10_50: @weak_variable = extern_weak global
// CHECK-10_51: @weak_variable = extern_weak global

func testObjCClass() {
  if #available(OSX 10.51, *) {
    let action = NSUserNotificationAction()
  }
}

func testGlobalVariable() {
  let i = weak_variable
}


@available(OSX 10.51, *)
func wrapWeakFunction() {
  // CHECK-10_50: declare extern_weak void @future_function_should_be_weak()
  // CHECK-10_51: declare void @future_function_should_be_weak()
  future_function_should_be_weak()
}
