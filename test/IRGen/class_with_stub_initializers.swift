// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -Xllvm -new-mangling-for-tests %s -emit-ir | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK:       @_T028class_with_stub_initializers3FooCN =
// -- The init() stub should get no vtable entry
// CHECK-NOT:     %C28class_with_stub_initializers3Foo* (%C28class_with_stub_initializers3Foo*)*
// CHECK:         %C28class_with_stub_initializers3Foo* (i64, %C28class_with_stub_initializers3Foo*)*
// -- The init() stub should get no vtable entry
// CHECK-NOT:     %C28class_with_stub_initializers3Foo* (%C28class_with_stub_initializers3Foo*)*
// CHECK:       {{^(@|define)}}
class Foo: NSObject {
  init(x: Int64) { super.init() }
}
