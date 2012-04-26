// RUN: %swift -triple x86_64-apple-darwin10 -I %S/.. %s -emit-llvm | FileCheck %s

import swift

// CHECK: [[REFCOUNT:%swift.refcounted]] = type
// CHECK: [[INT:%.*]] = type { { i64 } }

func test0(n : Int) -> SliceInt64 {
   return new Int[n]
}
// CHECK:    define { i8*, i64, [[REFCOUNT]]* } @_T3new5test0FT1nNSs5Int64_NSs10SliceInt64(i64 %n) {
// CHECK:      [[N:%.*]] = call i64 @_TNSs5Int6418getArrayBoundValuefRS_FT_i64([[INT]]*
// CHECK-NEXT: [[T0:%.*]] = call { i64, i1 } @llvm.umul.with.overflow.i64(i64 [[N]], i64 8) nounwind readnone
// CHECK-NEXT: [[T1:%.*]] = extractvalue { i64, i1 } [[T0]], 0
// CHECK-NEXT: [[T2:%.*]] = extractvalue { i64, i1 } [[T0]], 1
// CHECK-NEXT: [[S0:%.*]] = select i1 [[T2]], i64 -1, i64 [[T1]]
// CHECK-NEXT: [[T0:%.*]] = call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 [[S0]], i64 24) nounwind readnone
// CHECK-NEXT: [[T1:%.*]] = extractvalue { i64, i1 } [[T0]], 0
// CHECK-NEXT: [[T2:%.*]] = extractvalue { i64, i1 } [[T0]], 1
// CHECK-NEXT: [[S1:%.*]] = select i1 [[T2]], i64 -1, i64 [[T1]]
// CHECK-NEXT: [[ALLOC:%.*]] = call [[REFCOUNT]]* @swift_alloc({{%.*}} {{@.*}}, i64 [[S1]], i64 8)
// CHECK-NEXT: [[T0:%.*]] = bitcast [[REFCOUNT]]* [[ALLOC]] to i8*
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds i8* [[T0]], i32 24
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to [[INT]]*
// CHECK-NEXT: [[S2:%.*]] = sub i64 [[S1]], 24
// CHECK-NEXT: [[T3:%.*]] = bitcast [[INT]]* [[T2]] to  i8*
// CHECK-NEXT: call void @llvm.memset.p0i8.i64(i8* [[T3]], i8 0, i64 [[S2]], i32 8, i1 false)
// CHECK-NEXT: [[T4:%.*]] = bitcast [[INT]]* [[T2]] to  i8*
// CHECK-NEXT: call { i8*, i64, [[REFCOUNT]]* } @_TNSs10SliceInt6420convertFromHeapArrayFT4basep5ownero6lengthi64_S_(i8* [[T4]], [[REFCOUNT]]* [[ALLOC]], i64 [[N]])
// store to return slot, extract from return slot, return
// CHECK-NOT:  release
// CHECK:      ret { i8*, i64, [[REFCOUNT]]* }

// CHECK:    define internal i64 @arraydestroy{{.*}}([[REFCOUNT]]*) {
// CHECK:      [[T0:%.*]] = getelementptr inbounds [[REFCOUNT]]* %0, i32 1
// CHECK-NEXT: [[T1:%.*]] = bitcast [[REFCOUNT]]* [[T0]] to i64*
// CHECK-NEXT: [[LEN:%.*]] = load i64* [[T1]], align 8
// CHECK-NEXT: bitcast
// CHECK-NEXT: getelementptr
// CHECK-NEXT: bitcast
// CHECK-NEXT: getelementptr
// CHECK-NEXT: [[T0:%.*]] = mul i64 [[LEN]], 8
// CHECK-NEXT: [[T1:%.*]] = add i64 [[LEN]], 24
// CHECK-NEXT: ret i64 [[T1]]

// CHECK:    define internal i64 @arraysize{{.*}}([[REFCOUNT]]*) {
// CHECK:      [[T0:%.*]] = getelementptr inbounds [[REFCOUNT]]* %0, i32 1
// CHECK-NEXT: [[T1:%.*]] = bitcast [[REFCOUNT]]* [[T0]] to i64*
// CHECK-NEXT: [[LEN:%.*]] = load i64* [[T1]], align 8
// CHECK-NEXT: [[T0:%.*]] = mul i64 [[LEN]], 8
// CHECK-NEXT: [[T1:%.*]] = add i64 [[LEN]], 24
// CHECK-NEXT: ret i64 [[T1]]
