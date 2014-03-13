// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/Inputs %s -emit-silgen | FileCheck %s

import gizmo

// Although we don't ever expose initializers and methods of generic classes
// to ObjC yet, a generic subclass of an ObjC class must still use ObjC
// deallocation.

// CHECK-NOT: sil @_TFCSo7Genericd
// CHECK-NOT: sil @_TFCSo8NSObjectd

class Generic<T>: NSObject {
  var x: Int
}
