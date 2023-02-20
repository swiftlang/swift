// RUN: %target-swift-ide-test -print-module -module-to-print=CxxClassWithNSStringInit -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop -enable-objc-interop | %FileCheck -check-prefix=CHECK-IDE-TEST %s
// RUN: %target-swift-frontend -I %S/Inputs -enable-experimental-cxx-interop -emit-ir %s | %FileCheck %s


// REQUIRES: objc_interop

import Foundation
import CxxClassWithNSStringInit

// CHECK-IDE-TEST: struct S {
// CHECK-IDE-TEST:   init()
// CHECK-IDE-TEST:   init(A: NSString?, B: NSString?, C: NSString?)
// CHECK-IDE-TEST:   var A: NSString?
// CHECK-IDE-TEST:   var B: NSString?
// CHECK-IDE-TEST:   var C: NSString?
// CHECK-IDE-TEST: }

var foo: NSString? = "foo"
var bar: NSString? = "bar"
var baz: NSString? = "baz"
var s = S(A: foo, B: bar, C: baz)
s.dump()

// CHECK: call {{.*}} @_ZN1SC1ERKS_
// CHECK: call {{.*}} @_ZNK1S4dumpEv
