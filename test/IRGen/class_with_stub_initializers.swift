// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -target x86_64-apple-macosx10.9 -module-cache-path %t/clang-module-cache -sdk %S/Inputs -I=%S/Inputs -enable-source-import %s -emit-ir | FileCheck %s

import Foundation

// CHECK-LABEL: @_TMdC28class_with_stub_initializers3Foo = 
// CHECK:         %C28class_with_stub_initializers3Foo* (i64, %C28class_with_stub_initializers3Foo*)*
// -- The init() stub should get no vtable entry
// CHECK-NOT:     %C28class_with_stub_initializers3Foo* (%C28class_with_stub_initializers3Foo*)*
class Foo: NSObject {
  init(x: Int) { super.init() }
}
