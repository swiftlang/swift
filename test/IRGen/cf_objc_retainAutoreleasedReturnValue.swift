// RUN: %target-swift-frontend -module-name cf_objc_retainAutoreleasedReturnValue -I %S/Inputs %s -enable-objc-interop -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -module-name cf_objc_retainAutoreleasedReturnValue -I %S/Inputs %s -emit-ir | %FileCheck %s --check-prefix=NO_INTEROP

import CFBridgedType

@inline(never)
public func foo() {
  let _ = returnsACFBridgedType()
}

// With interop enabled, this should use objc_retainAutoreleasedReturnValue()

// CHECK-LABEL: define protected swiftcc void @"$s37cf_objc_retainAutoreleasedReturnValue3fooyyF"()
// CHECK: entry:
// CHECK:   %0 = call {{.*}}@returnsACFBridgedType()
// CHECK:   %1 = notail call ptr @llvm.objc.retainAutoreleasedReturnValue(ptr %0)

// Without interop, it should call swift_retain() instead.

// NO_INTEROP-LABEL: define protected swiftcc void @"$s37cf_objc_retainAutoreleasedReturnValue3fooyyF"()
// NO_INTEROP: entry:
// NO_INTEROP:   %0 = call {{.*}}@returnsACFBridgedType()
// NO_INTEROP:   %1 = call ptr @swift_retain(ptr returned %0)

