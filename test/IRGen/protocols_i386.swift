// RUN: %swift -triple i386-apple-darwin10 %s -emit-llvm | FileCheck %s

// CHECK: [[DOUBLE:%_TSs6Double]] = type { double }
// CHECK: [[SET:%_T14protocols_i3868Zeroable]] = type { i8**, [[BUFFER:.*]] }

// CHECK: [[WITNESS0:@witness_table.*]] = internal constant [13 x i8*] [i8* bitcast (void (i8*, i8*)* @__swift_noop_void_return to i8*), i8* bitcast (i8* ([[BUFFER]]*, [[BUFFER]]*, i8**)* @_TwCPNSs6Double to i8*), i8* bitcast (i8* ([[BUFFER]]*, i8**)* @_TwprNSs6Double to i8*), i8* bitcast (void (i8*, i8*)* @__swift_noop_void_return to i8*), i8* bitcast (void (i8*, i8*)* @__swift_noop_void_return to i8*), i8* bitcast (i8* ([[BUFFER]]*, i8*, i8**)* @_TwCpNSs6Double to i8*), i8* bitcast (i8* (i8*, i8*, i8*)* @__swift_memcpy8_8 to i8*), i8* bitcast (i8* (i8*, i8*, i8*)* @__swift_memcpy8_8 to i8*), i8* bitcast (i8* ([[BUFFER]]*, i8*, i8**)* @_TwTkNSs6Double to i8*), i8* bitcast (i8* (i8*, i8*, i8*)* @__swift_memcpy8_8 to i8*), i8* bitcast (i8* (i8*, i8*, i8*)* @__swift_memcpy8_8 to i8*), i8* bitcast (i8* ([[BUFFER]]*, i8**)* @_TwalNSs6Double to i8*), i8* bitcast ({ i32, i32 } (i8**)* @_TwsaNSs6Double to i8*)]

protocol Zeroable {
  func isZero() -> Bool
}

// This is an example designed to provoke dynamic alignment testing.
extension Double {
  func isZero() -> Bool { return this == 0 }
}
func test0(left : [byref] Zeroable) {
  left = Double(100)
}

// CHECK:    define void @_T14protocols_i3865test0FT4leftRPS_8Zeroable_T_([[SET]]* [[LEFT:%left]]) {
// CHECK:      [[TEMP:%.*]] = alloca [[SET]], align 4
//   Store the witness table in the temporary.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[SET]]* [[TEMP]], i32 0, i32 0
// CHECK-NEXT: store i8** getelementptr inbounds ([13 x i8*]* [[WITNESS0]], i32 0, i32 0), i8*** [[T0]], align 4
//   Evaluate the expression in-place in the temporary.
//   Note the realignment.
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[SET]]* [[TEMP]], i32 0, i32 1
// CHECK-NEXT: [[T2:%.*]] = ptrtoint [[BUFFER]]* [[T1]] to i32
// CHECK-NEXT: [[T3:%.*]] = add i32 [[T2]], 7
// CHECK-NEXT: [[T4:%.*]] = and i32 [[T3]], -8
// CHECK-NEXT: [[OBJ:%.*]] = inttoptr i32 [[T4]] to [[DOUBLE]]*
// CHECK-NEXT: [[V:%.*]] = call double @_TNSs6Double25convertFromIntegerLiteralFT1vi128_S_(i128 100)
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[DOUBLE]]* [[OBJ]], i32 0, i32 0
// CHECK-NEXT: store double [[V]], double* [[T0]], align 8
//   Destroy the current value in left.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[SET]]* [[LEFT]], i32 0, i32 0
// CHECK-NEXT: [[WT:%.*]] = load i8*** [[T0]], align 4
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[SET]]* [[LEFT]], i32 0, i32 1
// CHECK-NEXT: [[T1:%.*]] = load i8** [[WT]], align 4
// CHECK-NEXT: [[FN:%.*]] = bitcast i8* [[T1]] to void ([[BUFFER]]*, i8**)*
// CHECK-NEXT: call void [[FN]]([[BUFFER]]* [[T0]], i8** [[WT]]) nounwind
//   Move the new bits into place.
// CHECK-NEXT: [[T0:%.*]] = bitcast [[SET]]* [[LEFT]] to i8*
// CHECK-NEXT: [[T1:%.*]] = bitcast [[SET]]* [[TEMP]] to i8*
// CHECK-NEXT: call void @llvm.memcpy.p0i8.p0i8.i64(i8* [[T0]], i8* [[T1]], i64 20, i32 4, i1 false)
//   Finish without destroying the temporary.
// CHECK-NEXT: ret void

//   initializeBufferWithCopyOfBuffer implementation.
// CHECK:    define i8* @_TwCPNSs6Double([[BUFFER]]* %dest, [[BUFFER]]* %src, i8**) {
// CHECK:      [[T0:%.*]] = ptrtoint [[BUFFER]]* %dest to i32
// CHECK-NEXT: [[T1:%.*]] = add i32 [[T0]], 7
// CHECK-NEXT: [[T2:%.*]] = and i32 [[T1]], -8
// CHECK-NEXT: [[DEST:%.*]] = inttoptr i32 [[T2]] to [[DOUBLE]]*
// CHECK-NEXT: [[T0:%.*]] = ptrtoint [[BUFFER]]* %src to i32
// CHECK-NEXT: [[T1:%.*]] = add i32 [[T0]], 7
// CHECK-NEXT: [[T2:%.*]] = and i32 [[T1]], -8
// CHECK-NEXT: [[SRC:%.*]] = inttoptr i32 [[T2]] to [[DOUBLE]]*
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[DOUBLE]]* [[SRC]], i32 0, i32 0
// CHECK-NEXT: [[V:%.*]] = load double* [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[DOUBLE]]* [[DEST]], i32 0, i32 0
// CHECK-NEXT: store double [[V]], double* [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = bitcast [[DOUBLE]]* [[OBJ]] to i8*
// CHECK-NEXT: ret i8* [[T0]]

//   projectBuffer implementation.
// CHECK:    define i8* @_TwprNSs6Double([[BUFFER]]* %buffer, i8**) {
// CHECK:      [[T0:%.*]] = ptrtoint [[BUFFER]]* %buffer to i32
// CHECK-NEXT: [[T1:%.*]] = add i32 [[T0]], 7
// CHECK-NEXT: [[T2:%.*]] = and i32 [[T1]], -8
// CHECK-NEXT: [[OBJ:%.*]] = inttoptr i32 [[T2]] to [[DOUBLE]]*
// CHECK-NEXT: [[T0:%.*]] = bitcast [[DOUBLE]]* [[OBJ]] to i8*
// CHECK-NEXT: ret i8* [[T0]]

//   initializeBufferWithCopy implementation.
// CHECK:    define i8* @_TwCpNSs6Double([[BUFFER]]* %dest, i8* %src, i8**) {
// CHECK:      [[SRC:%.*]] = bitcast i8* %src to [[DOUBLE]]*
// CHECK-NEXT: [[T0:%.*]] = ptrtoint [[BUFFER]]* %dest to i32
// CHECK-NEXT: [[T1:%.*]] = add i32 [[T0]], 7
// CHECK-NEXT: [[T2:%.*]] = and i32 [[T1]], -8
// CHECK-NEXT: [[DEST:%.*]] = inttoptr i32 [[T2]] to [[DOUBLE]]*
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[DOUBLE]]* [[SRC]], i32 0, i32 0
// CHECK-NEXT: [[V:%.*]] = load double* [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[DOUBLE]]* [[DEST]], i32 0, i32 0
// CHECK-NEXT: store double [[V]], double* [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = bitcast [[DOUBLE]]* [[DEST]] to i8*
// CHECK-NEXT: ret i8* [[T0]]

//   initializeBufferWithTake implementation.
// CHECK:    define i8* @_TwTkNSs6Double([[BUFFER]]* %dest, i8* %src, i8**) {
// CHECK:      [[SRC:%.*]] = bitcast i8* %src to [[DOUBLE]]*
// CHECK-NEXT: [[T0:%.*]] = ptrtoint [[BUFFER]]* %dest to i32
// CHECK-NEXT: [[T1:%.*]] = add i32 [[T0]], 7
// CHECK-NEXT: [[T2:%.*]] = and i32 [[T1]], -8
// CHECK-NEXT: [[DEST:%.*]] = inttoptr i32 [[T2]] to [[DOUBLE]]*
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[DOUBLE]]* [[SRC]], i32 0, i32 0
// CHECK-NEXT: [[V:%.*]] = load double* [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[DOUBLE]]* [[DEST]], i32 0, i32 0
// CHECK-NEXT: store double [[V]], double* [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = bitcast [[DOUBLE]]* [[DEST]] to i8*
// CHECK-NEXT: ret i8* [[T0]]

//   allocateBuffer implementation.
// CHECK:    define i8* @_TwalNSs6Double([[BUFFER]]* %buffer, i8**) {
// CHECK:      [[T0:%.*]] = ptrtoint [[BUFFER]]* %buffer to i32
// CHECK-NEXT: [[T1:%.*]] = add i32 [[T0]], 7
// CHECK-NEXT: [[T2:%.*]] = and i32 [[T1]], -8
// CHECK-NEXT: [[OBJ:%.*]] = inttoptr i32 [[T2]] to [[DOUBLE]]*
// CHECK-NEXT: [[T0:%.*]] = bitcast [[DOUBLE]]* [[OBJ]] to i8*
// CHECK-NEXT: ret i8* [[T0]]

//  sizeAndAlignment implementation.
// CHECK:    define { i32, i32 } @_TwsaNSs6Double(i8**) {
// CHECK:      ret { i32, i32 } { i32 8, i32 8 }