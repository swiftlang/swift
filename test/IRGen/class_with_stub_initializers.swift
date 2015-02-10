// RUN: rm -rf %t && mkdir %t
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) %s -emit-ir | FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK:       @_TMd[[FOO:C28class_with_stub_initializers3Foo]] = 
// -- The init() stub should get no vtable entry
// CHECK-NOT:     %[[FOO]]* (%[[FOO]]*)*
// CHECK:         %[[FOO]]* (i64, %[[FOO]]*)*
// -- The init() stub should get no vtable entry
// CHECK-NOT:     %[[FOO]]* (%[[FOO]]*)*
// CHECK:       {{^(@|define)}}
class Foo: NSObject {
  init(x: Int64) { super.init() }
}
