// RUN: rm -rf %t && mkdir %t
// RUN: %build-irgen-test-overlays
//
// Specify explicit target triples for the deployment target to test weak
// linking for a symbol introduced in OS X 10.10.
//
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.9 -sdk %S/Inputs -I %t -primary-file %s -emit-ir | FileCheck -check-prefix=CHECK-10_9 %s
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.10 -sdk %S/Inputs -I %t -primary-file %s -emit-ir | FileCheck -check-prefix=CHECK-10_10 %s

// REQUIRES: OS=macosx

// FIXME: This test in written in Swift because the SIL parser fails
// when referencing weak_variable.

import Foundation

// CHECK-10_9: @weak_variable = extern_weak global
// CHECK-10_10: @weak_variable = extern_weak global

// CHECK-10_9: @"OBJC_CLASS_$_NSUserNotificationAction" = extern_weak global %objc_class
// CHECK-10_10: @"OBJC_CLASS_$_NSUserNotificationAction" = external global %objc_class

func testObjCClass() {
  let action = NSUserNotificationAction()
}

func testGlobalVariable() {
  let i = weak_variable
}
