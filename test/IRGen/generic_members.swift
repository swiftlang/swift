// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

// CHECK: [[METADATA:%swift.heapmetadata]] = type {
// CHECK: [[REFCOUNT:%swift.refcounted]] = type { [[METADATA]]*, i64 }
// CHECK: [[INT:%_TSs5Int64]] = type { i64 }
// CHECK: [[DOUBLE:%_TSs6Double]] = type { double }
// CHECK: [[TEST0:%_T15generic_members5Test0]] = type { [[INT]] }
// CHECK: [[TEST1:%_T15generic_members5Test1]] = type { [[REFCOUNT]], [[INT]] }

func use(x : Int) {}
func use(x : Double) {}

struct Test0<T> {
  var x : Int
}
func test0a() {
  var y : Test0<Int>
  use(y.x)
}
// CHECK:    define void @_T15generic_members6test0aFT_T_() {
// CHECK:      [[Y:%.*]] = alloca [[TEST0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[TEST0]]* [[Y]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[T0]], i32 0, i32 0
// CHECK-NEXT: store i64 0, i64* [[T1]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[TEST0]]* [[Y]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = load i64* [[T1]], align 8
// CHECK-NEXT: call void @_T15generic_members3useFT1xSi_T_(i64 [[T2]]
// CHECK-NEXT: ret void

func test0b<T>() {
  var y : Test0<T>
  use(y.x)
}
// CHECK:    define void @_T15generic_members6test0bU__FT_T_(i8** %T) {
// CHECK:      [[Y:%.*]] = alloca [[TEST0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[TEST0]]* [[Y]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[T0]], i32 0, i32 0
// CHECK-NEXT: store i64 0, i64* [[T1]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[TEST0]]* [[Y]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = load i64* [[T1]], align 8
// CHECK-NEXT: call void @_T15generic_members3useFT1xSi_T_(i64 [[T2]]
// CHECK-NEXT: ret void

class Test1<T> {
  var x : Int
}
func test1a() {
  var y : Test1<Int>
  use(y.x)
}
// CHECK:    define void @_T15generic_members6test1aFT_T_() {
// CHECK:      [[Y:%.*]] = alloca [[TEST1]]*, align 8
// CHECK-NEXT: store [[TEST1]]* null, [[TEST1]]** [[Y]], align 8
// CHECK-NEXT: [[V:%.*]] = load [[TEST1]]** [[Y]], align 8
// CHECK-NEXT: [[T0:%.*]] = bitcast [[TEST1]]* [[V]] to [[REFCOUNT]]*
// CHECK-NEXT: call void @swift_retain_noresult([[REFCOUNT]]* [[T0]]) nounwind
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[TEST1]]* [[V]], i32 0, i32 1
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = load i64* [[T1]], align 8
// CHECK-NEXT: call void @_T15generic_members3useFT1xSi_T_(i64 [[T2]]
// CHECK-NEXT: call void bitcast (void ([[REFCOUNT]]*)* @swift_release to void ([[TEST1]]*)*)([[TEST1]]* [[V]]) nounwind
// CHECK-NEXT: [[T0:%.*]] = load [[TEST1]]** [[Y]], align 8
// CHECK-NEXT: call void bitcast (void ([[REFCOUNT]]*)* @swift_release to void ([[TEST1]]*)*)([[TEST1]]* [[T0]]) nounwind
// CHECK-NEXT: ret void

func test1b<T>() {
  var y : Test1<T>
  use(y.x)
}
// CHECK:    define void @_T15generic_members6test1bU__FT_T_(i8** %T) {
// CHECK:      [[Y:%.*]] = alloca [[TEST1]]*, align 8
// CHECK-NEXT: store [[TEST1]]* null, [[TEST1]]** [[Y]], align 8
// CHECK-NEXT: [[V:%.*]] = load [[TEST1]]** [[Y]], align 8
// CHECK-NEXT: [[T0:%.*]] = bitcast [[TEST1]]* [[V]] to [[REFCOUNT]]*
// CHECK-NEXT: call void @swift_retain_noresult([[REFCOUNT]]* [[T0]]) nounwind
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[TEST1]]* [[V]], i32 0, i32 1
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = load i64* [[T1]], align 8
// CHECK-NEXT: call void @_T15generic_members3useFT1xSi_T_(i64 [[T2]]
// CHECK-NEXT: call void bitcast (void ([[REFCOUNT]]*)* @swift_release to void ([[TEST1]]*)*)([[TEST1]]* [[V]]) nounwind
// CHECK-NEXT: [[T0:%.*]] = load [[TEST1]]** [[Y]], align 8
// CHECK-NEXT: call void bitcast (void ([[REFCOUNT]]*)* @swift_release to void ([[TEST1]]*)*)([[TEST1]]* [[T0]]) nounwind
// CHECK-NEXT: ret void
