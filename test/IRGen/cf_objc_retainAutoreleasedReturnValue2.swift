// RUN: %target-swift-frontend -module-name cf_objc_retainAutoreleasedReturnValue -I %S/Inputs %s -disable-objc-interop -emit-ir | %FileCheck %s

// This is in a separate file because *this* one works on WASM.
// (cf_objc_retainAutoreleasedReturnValue.swift does not.)

import CFBridgedType

@inline(never)
public func foo() {
  let _ = returnsACFBridgedType()
}

// With interop disabled, this should use swift_retain(). 

// CHECK-LABEL: define {{.*}}swiftcc void @"$s37cf_objc_retainAutoreleasedReturnValue3fooyyF"()
// CHECK: entry:
// CHECK:   %0 = call {{.*}}@returnsACFBridgedType()
// CHECK:   %1 = call ptr @swift_retain(ptr returned %0)
