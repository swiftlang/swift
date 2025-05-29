// RUN: %target-swift-frontend -module-name cf_objc_retainAutoreleasedReturnValue -I %S/Inputs %s -enable-objc-interop -emit-ir | %FileCheck %s

// We need to require objc_codegen to avoid this test on WASM.
// (That's why the other half of this test is in a separate file.)

// REQUIRES: objc_codegen

import CFBridgedType

@inline(never)
public func foo() {
  let _ = returnsACFBridgedType()
}

// With interop enabled, this should use objc_retainAutoreleasedReturnValue()

// CHECK-LABEL: define {{.*}}swiftcc void @"$s37cf_objc_retainAutoreleasedReturnValue3fooyyF"()
// CHECK: entry:
// CHECK:   %0 = call {{.*}}@returnsACFBridgedType()
// CHECK:   %1 = {{.*}}call ptr @llvm.objc.retainAutoreleasedReturnValue(ptr %0)
