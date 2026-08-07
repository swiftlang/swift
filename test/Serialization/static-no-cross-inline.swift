// --- Static case: StaticC built with -static. f3's body must be not be deserialized
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -O -static -parse-as-library \
// RUN:   -module-name StaticC %t/StaticC.swift \
// RUN:   -emit-module-path %t/StaticC.swiftmodule
// RUN: %target-swift-frontend -emit-module -O -parse-as-library \
// RUN:   -module-name DynamicB -I %t %t/DynamicB.swift \
// RUN:   -emit-module-path %t/DynamicB.swiftmodule
// RUN: %target-sil-opt -enable-sil-verify-all -I %t %t/DynamicB.swiftmodule \
// RUN:   -emit-sorted-sil -o - | %FileCheck %s --check-prefix=CHECK-STATIC

// --- Control case: StaticC built without -static. f3's body is deserialized
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -O -parse-as-library \
// RUN:   -module-name StaticC %t/StaticC.swift \
// RUN:   -emit-module-path %t/StaticC.swiftmodule
// RUN: %target-swift-frontend -emit-module -O -parse-as-library \
// RUN:   -module-name DynamicB -I %t %t/DynamicB.swift \
// RUN:   -emit-module-path %t/DynamicB.swiftmodule
// RUN: %target-sil-opt -enable-sil-verify-all -I %t %t/DynamicB.swiftmodule \
// RUN:   -emit-sorted-sil -o - | %FileCheck %s --check-prefix=CHECK-DYNAMIC

// In the static case, f3's body is not deserialized:
// CHECK-STATIC:     sil {{.*}}@$s7StaticC2f3SiyF{{.*}} -> Int {
// CHECK-STATIC-NEXT: [global: ]
// CHECK-STATIC-NEXT: } // end sil function '$s7StaticC2f3SiyF'
// CHECK-STATIC: function_ref @$s7StaticC2f3SiyF

// In the control case, f3's body is deserialized, and fully folded away
// CHECK-DYNAMIC-NOT: sil {{.*}}@$s7StaticC2f3SiyF{{.*}} -> Int{{[[:space:]]*$}}
// CHECK-DYNAMIC-NOT: function_ref @$s7StaticC2f3SiyF
// CHECK-DYNAMIC: integer_literal {{.*}} 43

//--- StaticC.swift

@inlinable
public func f3() -> Int {
  return 42
}

//--- DynamicB.swift

import StaticC

@inlinable
public func f2() -> Int {
  return f3() &+ 1
}
