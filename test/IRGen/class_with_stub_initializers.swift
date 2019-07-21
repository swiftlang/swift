// RUN: %empty-directory(%t)
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) %s -emit-ir | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK:       @"$s28class_with_stub_initializers3FooCN" =
// -- The init() stub should get no vtable entry
// CHECK-NOT:     %T28class_with_stub_initializers3FooC* (%swift.type*)*
// CHECK:         %T28class_with_stub_initializers3FooC* (i64, %swift.type*)*
// -- The init() stub should get no vtable entry
// CHECK-NOT:     %T28class_with_stub_initializers3FooC* (%swift.type*)*
// CHECK:       {{^(@|define)}}
class Foo: NSObject {
  init(x: Int64) { super.init() }
}
