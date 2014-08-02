// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path %t/clang-module-cache -target x86_64-apple-macosx10.9 -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-ir | FileCheck -check-prefix=CHECK-10_9 %s
// RUN: %swift -module-cache-path %t/clang-module-cache -target x86_64-apple-macosx10.10 -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-ir | FileCheck -check-prefix=CHECK-10_10 %s

// FIXME: This test in written in Swift because the SIL parser fails
// when referencing weak_variable.

import Foundation

// CHECK-10_9: @"OBJC_CLASS_$_NSUserNotificationAction" = extern_weak global %objc_class
// CHECK-10_10: @"OBJC_CLASS_$_NSUserNotificationAction" = external global %objc_class

// CHECK-10_9: @weak_variable = extern_weak global
// CHECK-10_10: @weak_variable = extern_weak global

func testObjCClass() {
  let action = NSUserNotificationAction()
}

func testGlobalVariable() {
  let i = weak_variable
}
