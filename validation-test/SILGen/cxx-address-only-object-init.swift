// RUN: %target-swift-frontend -enable-cxx-interop -I %S/Inputs %s -emit-silgen | %FileCheck %s

import CXXTypes

// Just make sure we create the object and don't crash.
// CHECK-LABEL: @$s4main4testyyF
// CHECK: alloc_stack
// CHECK: apply
// CHECK: return %{{[0-9]+}} : $()
public func test() {
  let c = HasCustomCopyConst()
}
