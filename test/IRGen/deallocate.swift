// RUN: %target-swift-frontend -emit-ir %s | FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

// rdar://16979846

class CustomDeallocator {
  func __getInstanceSizeAndAlignMask() -> (Int, Int) {
    return (48, 15)
  }
}

// CHECK:    define hidden void @_TFC10deallocate17CustomDeallocatorD([[CD:%.*]]*
// CHECK:      [[T0:%.*]] = call [[OBJECT:%.*]]* @_TFC10deallocate17CustomDeallocatord(
// CHECK-NEXT: [[T1:%.*]] = bitcast [[OBJECT]]* [[T0]] to [[CD]]*
// CHECK-NEXT: [[T2:%.*]] = bitcast [[CD]]* [[T1]] to [[OBJECT]]*
// CHECK-NEXT: call void @swift_retain_noresult([[OBJECT]]* [[T2]])
// CHECK-NEXT: [[T3:%.*]] = call { i64, i64 } @_TFC10deallocate17CustomDeallocator29__getInstanceSizeAndAlignMaskfS0_FT_TSiSi_([[CD]]* [[T1]])
// CHECK-NEXT: [[SIZE:%.*]] = extractvalue { i64, i64 } [[T3]], 0
// CHECK-NEXT: [[ALIGNMASK:%.*]] = extractvalue { i64, i64 } [[T3]], 1
// CHECK-NEXT: [[T4:%.*]] = bitcast [[CD]]* [[T1]] to [[OBJECT]]*
// CHECK-NEXT: call void @swift_deallocClassInstance([[OBJECT]]* [[T4]], i64 [[SIZE]], i64 [[ALIGNMASK]])
// CHECK-NEXT: ret void
