// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir %s | %FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

// rdar://16979846

class CustomDeallocator {
  func __getInstanceSizeAndAlignMask() -> (Int, Int) {
    return (48, 15)
  }
}

// CHECK:    define hidden swiftcc void @_T010deallocate17CustomDeallocatorCfD([[CD:%.*]]* swiftself
// CHECK:      [[T0:%.*]] = call swiftcc [[OBJECT:%.*]]* @_T010deallocate17CustomDeallocatorCfd(
// CHECK-NEXT: [[T1:%.*]] = bitcast [[OBJECT]]* [[T0]] to [[CD]]*
// CHECK-NEXT: [[T3:%.*]] = call swiftcc { i64, i64 } @_T010deallocate17CustomDeallocatorC29__getInstanceSizeAndAlignMaskSi_SityF([[CD]]* [[T1]])
// CHECK-NEXT: [[SIZE:%.*]] = extractvalue { i64, i64 } [[T3]], 0
// CHECK-NEXT: [[ALIGNMASK:%.*]] = extractvalue { i64, i64 } [[T3]], 1
// CHECK-NEXT: [[T4:%.*]] = bitcast [[CD]]* [[T1]] to [[OBJECT]]*
// CHECK-NEXT: call void @swift_deallocClassInstance([[OBJECT]]* [[T4]], i64 [[SIZE]], i64 [[ALIGNMASK]])
// CHECK-NEXT: ret void
