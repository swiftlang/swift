// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -triple x86_64-apple-darwin10 -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-llvm | FileCheck %s

// CHECK: [[TYPE:%swift.type]] = type
// CHECK: [[SGIZMO:%_T13objc_subclass10SwiftGizmo]] = type
// CHECK: [[INT:%_TSs5Int64]] = type { i64 }
// CHECK: [[OBJC:%objc_object]] = type opaque

import gizmo

class SwiftGizmo : Gizmo {
  var x : Int

  func getX() -> Int {
    return x;
  }
}


// CHECK:    define i64 @_TC13objc_subclass10SwiftGizmo4getXfS0_FT_Si([[SGIZMO]]* %this) {
// CHECK:      [[RET:%.*]] = alloca [[INT]], align 8
// CHECK-NEXT: [[THIS:%.*]] = alloca [[SGIZMO]]*, align 8
// CHECK-NEXT: store [[SGIZMO]]* %this, [[SGIZMO]]** [[THIS]], align 8
// CHECK-NEXT: [[T0:%.*]] = load [[SGIZMO]]** [[THIS]], align 8
// CHECK-NEXT: call [[SGIZMO]]* bitcast ([[OBJC]]* ([[OBJC]]*)* @objc_retain to [[SGIZMO]]* ([[SGIZMO]]*)*)([[SGIZMO]]* [[T0]]) nounwind
// CHECK-NEXT: [[T1:%.*]] = load i64* @_TWvdC13objc_subclass10SwiftGizmo1xSi, align 8
// CHECK-NEXT: [[T2:%.*]] = bitcast [[SGIZMO]]* [[T0]] to i8*
// CHECK-NEXT: [[T3:%.*]] = getelementptr inbounds i8* [[T2]], i64 [[T1]]
// CHECK-NEXT: [[T4:%.*]] = bitcast i8* [[T3]] to [[INT]]*
// CHECK-NEXT: [[T5:%.*]] = getelementptr inbounds [[INT]]* [[T4]], i32 0, i32 0
// CHECK-NEXT: [[T6:%.*]] = load i64* [[T5]], align 8
// CHECK-NEXT: [[T7:%.*]] = getelementptr inbounds [[INT]]* [[RET]], i32 0, i32 0
// CHECK-NEXT: store i64 [[T6]], i64* [[T7]], align 8
