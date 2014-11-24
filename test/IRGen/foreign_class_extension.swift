// RUN: rm -rf %t && mkdir %t
// RUN: %build-irgen-test-overlays
// RUN: %swift -target x86_64-apple-macosx10.9 -module-cache-path %t/clang-module-cache -sdk %S/Inputs -I %t %s -emit-ir | FileCheck %s
// XFAIL: linux

import Foundation

// We can't emit categories when extending CF types because they don't have
// real class metadata.
// CHECK-NOT: @"OBJC_CLASS_$_CGImage"

extension CGImage {
  func foo() {}
  var bar: Int { return 0 }
  subscript(x: Int) -> Int { return x }
}
