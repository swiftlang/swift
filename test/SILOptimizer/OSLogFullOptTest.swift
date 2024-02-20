// RUN: %target-swift-frontend -emit-ir -swift-version 5 -O -enable-copy-propagation -enable-lexical-lifetimes=false -primary-file %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize
//
// REQUIRES: VENDOR=apple
// REQUIRES: swift_stdlib_no_asserts
// REQUIRES: PTRSIZE=64
// REQUIRES: swift_in_compiler

// REQUIRES: rdar97728676

// This tests the optimality of the IR generated for the new os log APIs. This
// is not testing the output of a specific optimization pass (which has separate
// tests) but that all optimizations together result in optimal IR. If this test
// fails, it implies that some expected optimizations fail to get triggered on
// os log APIs. TODO: eventually these optimization should also happen in Onone
// mode.

// With stdlib asserts, this test exhibits the same problem as
// dead_array_elim.swift. The problem can be overcome by handling
// non-trivial stores in OSSA, as described here:
//   [OSSA] Improve DeadObjectElimination to handle array copies
//   https://github.com/apple/swift/issues/56179
// Once that bug is fixed, remove the requirement: swift_stdlib_no_asserts.

import OSLogTestHelper
import Foundation

// CHECK-LABEL: define hidden swiftcc void @"${{.*}}testSimpleInterpolation
func testSimpleInterpolation() {
  _osLogTestHelper("Minimum integer value: \(Int.min)")
    // CHECK: entry:
    // Ignore some code related to the default argument and string literal invariant
    // checks.
    // CHECK: tail call swiftcc i1 @"${{.*}}isLoggingEnabled{{.*}}"()
    // CHECK-NEXT: br i1 {{%.*}}, label %[[ENABLED:[0-9]+]], label %[[NOT_ENABLED:[0-9]+]]

    // CHECK: [[ENABLED]]:
    //
    // Header bytes.
    //
    // CHECK-64: [[BUFFER:%.+]] = tail call noalias i8* @swift_slowAlloc(i64 12
    // CHECK-32: [[BUFFER:%.+]] = tail call noalias i8* @swift_slowAlloc(i32 8
    // CHECK-NEXT: store i8 0, i8* [[BUFFER]], align 1
    // CHECK-NEXT: [[OFFSET1:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 1
    // CHECK-NEXT: store i8 1, i8* [[OFFSET1]], align 1
    //
    // Argument bytes.
    //
    // CHECK-NEXT: [[OFFSET2:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 2
    // CHECK-NEXT: store i8 0, i8* [[OFFSET2]], align 1
    // CHECK-NEXT: [[OFFSET3:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 3
    // CHECK-64-NEXT: store i8 8, i8* [[OFFSET3]], align 1
    // CHECK-32-NEXT: store i8 4, i8* [[OFFSET3]], align 1
    // CHECK-NEXT: [[OFFSET4:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 4
    // CHECK-NEXT: [[BITCASTED:%.+]] = bitcast i8* [[OFFSET4]] to i{{.*}}*
    // CHECK-64-NEXT: store i64 -9223372036854775808, i64* [[BITCASTED]], align 1
    // CHECK-32-NEXT: store i32 -2147483648, i32* [[BITCASTED]], align 1
    // CHECK-64-NEXT: tail call swiftcc void @"${{.*}}_os_log_impl_test{{.*}}"({{.*}}, {{.*}}, {{.*}}, {{.*}}, i8* {{(nonnull )?}}getelementptr inbounds ([27 x i8], [27 x i8]* @{{.*}}, i64 0, i64 0), i8* {{(nonnull )?}}[[BUFFER]], i32 12)
    // CHECK-32-NEXT: tail call swiftcc void @"${{.*}}_os_log_impl_test{{.*}}"({{.*}}, {{.*}}, {{.*}}, {{.*}}, i8* {{(nonnull )?}}getelementptr inbounds ([27 x i8], [27 x i8]* @{{.*}}, i32 0, i32 0), i8* {{(nonnull )?}}[[BUFFER]], i32 8)
    // CHECK-NEXT: tail call void @swift_slowDealloc(i8* {{(nonnull )?}}[[BUFFER]]
    // CHECK-NEXT: br label %[[NOT_ENABLED]]
  
    // CHECK: [[NOT_ENABLED]]:
    // CHECK-NEXT: tail call void @swift_release
    // CHECK-NEXT: ret void
}

// CHECK-LABEL: define hidden swiftcc void @"${{.*}}testInterpolationWithMultipleArguments
func testInterpolationWithMultipleArguments() {
  let privateID: Int32 = 0x79abcdef
  let filePermissions: Int32 = 0o777
  let pid: Int32 = 122225
  _osLogTestHelper(
    """
    Access prevented: process \(pid, privacy: .public) initiated by \
    user: \(privateID, privacy: .private) attempted resetting \
    permissions to \(filePermissions)
    """)
    // CHECK: entry:
    // CHECK: tail call swiftcc i1 @"${{.*}}isLoggingEnabled{{.*}}"()
    // CHECK-NEXT: br i1 {{%.*}}, label %[[ENABLED:[0-9]+]], label %[[NOT_ENABLED:[0-9]+]]

    // CHECK: [[ENABLED]]:
    //
    // Header bytes.
    //
    // CHECK: [[BUFFER:%.+]] = tail call noalias i8* @swift_slowAlloc(i{{.*}} 20
    // CHECK-NEXT: store i8 1, i8* [[BUFFER]], align 1
    // CHECK-NEXT: [[OFFSET1:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 1
    // CHECK-NEXT: store i8 3, i8* [[OFFSET1]], align 1
    //
    // First argument bytes.
    //
    // CHECK-NEXT: [[OFFSET2:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 2
    // CHECK-NEXT: store i8 2, i8* [[OFFSET2]], align 1
    // CHECK-NEXT: [[OFFSET3:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 3
    // CHECK-NEXT: store i8 4, i8* [[OFFSET3]], align 1
    // CHECK-NEXT: [[OFFSET4:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 4
    // CHECK-NEXT: [[BITCASTED:%.+]] = bitcast i8* [[OFFSET4]] to i32*
    // CHECK-NEXT: store i32 122225, i32* [[BITCASTED]], align 1
    //
    // Second argument
    //
    // CHECK-NEXT: [[OFFSET12:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 8
    // CHECK-NEXT: store i8 1, i8* [[OFFSET12]], align 1
    // CHECK-NEXT: [[OFFSET13:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 9
    // CHECK-NEXT: store i8 4, i8* [[OFFSET13]], align 1
    // CHECK-NEXT: [[OFFSET14:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 10
    // CHECK-NEXT: [[BITCASTED2:%.+]] = bitcast i8* [[OFFSET14]] to i32*
    // CHECK-NEXT: store i32 2041302511, i32* [[BITCASTED2]], align 1
    //
    // Third argument
    //
    // CHECK-NEXT: [[OFFSET22:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 14
    // CHECK-NEXT: store i8 0, i8* [[OFFSET22]], align 1
    // CHECK-NEXT: [[OFFSET23:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 15
    // CHECK-NEXT: store i8 4, i8* [[OFFSET23]], align 1
    // CHECK-NEXT: [[OFFSET24:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 16
    // CHECK-NEXT: [[BITCASTED3:%.+]] = bitcast i8* [[OFFSET24]] to i32*
    // CHECK-NEXT: store i32 511, i32* [[BITCASTED3]], align 1
    //
    // os_log_impl call.
    // CHECK-NEXT: tail call swiftcc void @"${{.*}}_os_log_impl_test{{.*}}"({{.*}}, {{.*}}, {{.*}}, {{.*}}, i8* {{(nonnull )?}}getelementptr inbounds ([106 x i8], [106 x i8]* @{{.*}}, i{{.*}} 0, i{{.*}} 0), i8* {{(nonnull )?}}[[BUFFER]], i32 20)
    // CHECK-NEXT: tail call void @swift_slowDealloc(i8* {{(nonnull )?}}[[BUFFER]]
    // CHECK-NEXT: br label %[[NOT_ENABLED]]

    // CHECK: [[NOT_ENABLED]]:
    // CHECK-NEXT: tail call void @swift_release
    // CHECK-NEXT: ret void
}

// CHECK-LABEL: define hidden swiftcc void @"${{.*}}testNSObjectInterpolation
func testNSObjectInterpolation(nsArray: NSArray) {
  _osLogTestHelper("NSArray: \(nsArray, privacy: .public)")
    // TODO: check why the ARC optimizer cannot eliminate the many retain/release pairs here.
    // CHECK: entry:
    // CHECK-NEXT: bitcast %TSo7NSArrayC* %0 to i8*
    // CHECK-NEXT: [[COPY:%.+]] = tail call i8* @llvm.objc.retain
    // CHECK-NEXT: [[NSARRAY_ARG:%.+]] = tail call i8* @llvm.objc.retain
    // CHECK: tail call swiftcc i1 @"${{.*}}isLoggingEnabled{{.*}}"()
    // CHECK-NEXT: br i1 {{%.*}}, label %[[ENABLED:[0-9]+]], label %[[NOT_ENABLED:[0-9]+]]

    // CHECK: [[NOT_ENABLED]]:
    // CHECK-NEXT: tail call void @swift_release
    // CHECK-NEXT: tail call void @llvm.objc.release
    // CHECK-NEXT: tail call void @llvm.objc.release
    // CHECK: br label %[[EXIT:[0-9]+]]

    // CHECK: [[ENABLED]]:
    //
    // Header bytes.
    //
    // CHECK-64: [[BUFFER:%.+]] = tail call noalias i8* @swift_slowAlloc(i64 12
    // CHECK-32: [[BUFFER:%.+]] = tail call noalias i8* @swift_slowAlloc(i32 8
    // CHECK-64: [[OBJ_STORAGE:%.+]] = tail call noalias i8* @swift_slowAlloc(i64 8
    // CHECK-32: [[OBJ_STORAGE:%.+]] = tail call noalias i8* @swift_slowAlloc(i32 4
    // CHECK: store i8 2, i8* [[BUFFER]], align 1
    // CHECK-NEXT: [[OFFSET1:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 1
    // CHECK-NEXT: store i8 1, i8* [[OFFSET1]], align 1
    //
    // Argument bytes.
    //
    // CHECK-NEXT: [[OFFSET2:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 2
    // CHECK-NEXT: store i8 66, i8* [[OFFSET2]], align 1
    // CHECK-NEXT: [[OFFSET3:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 3
    // CHECK-64-NEXT: store i8 8, i8* [[OFFSET3]], align 1
    // CHECK-32-NEXT: store i8 4, i8* [[OFFSET3]], align 1
    // CHECK-NEXT: [[OFFSET4:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 4
    // CHECK-NEXT: [[BITCASTED_DEST:%.+]] = bitcast i8* [[OFFSET4]] to %TSo7NSArrayC**
    // CHECK-NEXT: [[BITCASTED_SRC:%.+]] = bitcast i8* {{.*}} to %TSo7NSArrayC*
    // CHECK-NEXT: store %TSo7NSArrayC*  [[BITCASTED_SRC]], %TSo7NSArrayC** [[BITCASTED_DEST]], align 1
    // CHECK-NEXT: [[BITCASTED_DEST2:%.+]] = bitcast i8* [[OBJ_STORAGE]] to %TSo7NSArrayC**
    // CHECK-NEXT: [[BITCASTED_SRC2:%.+]] = bitcast i8* {{.*}} to %TSo7NSArrayC*
    // CHECK-64-NEXT: store %TSo7NSArrayC* [[BITCASTED_SRC2]], %TSo7NSArrayC** [[BITCASTED_DEST2]], align 8
    // CHECK-32-NEXT: store %TSo7NSArrayC* [[BITCASTED_SRC2]], %TSo7NSArrayC** [[BITCASTED_DEST2]], align 4
    // CHECK-NEXT: tail call void @llvm.objc.release(i8* [[NSARRAY_ARG]])
    // CHECK-64: tail call swiftcc void @"${{.*}}_os_log_impl_test{{.*}}"({{.*}}, {{.*}}, {{.*}}, {{.*}}, i8* {{(nonnull )?}}getelementptr inbounds ([20 x i8], [20 x i8]* @{{.*}}, i64 0, i64 0), i8* {{(nonnull )?}}[[BUFFER]], i32 12)
    // CHECK-32: tail call swiftcc void @"${{.*}}_os_log_impl_test{{.*}}"({{.*}}, {{.*}}, {{.*}}, {{.*}}, i8* {{(nonnull )?}}getelementptr inbounds ([20 x i8], [20 x i8]* @{{.*}}, i32 0, i32 0), i8* {{(nonnull )?}}[[BUFFER]], i32 8)
    // CHECK:      [[BITCASTED_OBJ_STORAGE:%.+]] = bitcast i8* [[OBJ_STORAGE]] to %swift.opaque*
    // CHECK-NEXT: tail call void @swift_arrayDestroy(%swift.opaque* [[BITCASTED_OBJ_STORAGE]]
    // CHECK-NEXT: tail call void @swift_slowDealloc(i8* {{(nonnull )?}}[[OBJ_STORAGE]]
    // CHECK-NEXT: tail call void @swift_slowDealloc(i8* {{(nonnull )?}}[[BUFFER]]
    // CHECK-NEXT: tail call void @swift_release
    // CHECK-NEXT: br label %[[EXIT]]
  
    // CHECK: [[EXIT]]:
    // CHECK-NEXT: ret void
}

// CHECK-LABEL: define hidden swiftcc void @"${{.*}}testFloatInterpolation
func testFloatInterpolation(doubleValue: Double) {
  _osLogTestHelper("Double value: \(doubleValue)")
    // CHECK: entry:
    // CHECK: tail call swiftcc i1 @"${{.*}}isLoggingEnabled{{.*}}"()
    // CHECK-NEXT: br i1 {{%.*}}, label %[[ENABLED:[0-9]+]], label %[[NOT_ENABLED:[0-9]+]]

    // CHECK: [[ENABLED]]:
    //
    // Header bytes.
    //
    // CHECK: [[BUFFER:%.+]] = tail call noalias i8* @swift_slowAlloc(i{{.*}} 12
    // CHECK-NEXT: store i8 0, i8* [[BUFFER]], align 1
    // CHECK-NEXT: [[OFFSET1:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 1
    // CHECK-NEXT: store i8 1, i8* [[OFFSET1]], align 1
    //
    // Argument bytes.
    //
    // CHECK-NEXT: [[OFFSET2:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 2
    // CHECK-NEXT: store i8 0, i8* [[OFFSET2]], align 1
    // CHECK-NEXT: [[OFFSET3:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 3
    // CHECK-NEXT: store i8 8, i8* [[OFFSET3]], align 1
    // CHECK-NEXT: [[OFFSET4:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 4
    // CHECK-NEXT: [[BITCASTED:%.+]] = bitcast i8* [[OFFSET4]] to double*
    // CHECK-NEXT: store double %0, double* [[BITCASTED]], align 1
    // CHECK-NEXT: tail call swiftcc void @"${{.*}}_os_log_impl_test{{.*}}"({{.*}}, {{.*}}, {{.*}}, {{.*}}, i8* {{(nonnull )?}}getelementptr inbounds ([17 x i8], [17 x i8]* @{{.*}}, i{{.*}} 0, i{{.*}} 0), i8* {{(nonnull )?}}[[BUFFER]], i32 12)
    // CHECK-NEXT: tail call void @swift_slowDealloc(i8* {{(nonnull )?}}[[BUFFER]]
    // CHECK-NEXT: br label %[[NOT_ENABLED]]

    // CHECK: [[NOT_ENABLED]]:
    // CHECK-NEXT: tail call void @swift_release
    // CHECK-NEXT: ret void
}

// This test checks that the precision and alignment are optimally "stored" into the
// byte buffer at the right positions.
// CHECK-LABEL: define hidden swiftcc void @"${{.*}}testDynamicPrecisionAndAlignment
func testDynamicPrecisionAndAlignment() {
  _osLogTestHelper(
    """
     Maximum Int64 value: \
     \(Int32.max, format: .decimal(minDigits: 10), align: .left(columns: 5))
     """)
    // CHECK: entry:
    // CHECK: tail call swiftcc i1 @"${{.*}}isLoggingEnabled{{.*}}"()
    // CHECK-NEXT: br i1 {{%.*}}, label %[[ENABLED:[0-9]+]], label %[[NOT_ENABLED:[0-9]+]]

    // CHECK: [[ENABLED]]:
    //
    // Header bytes.
    //
    // CHECK: [[BUFFER:%.+]] = tail call noalias i8* @swift_slowAlloc(i{{.*}} 20
    // CHECK-NEXT: store i8 0, i8* [[BUFFER]], align 1
    // CHECK-NEXT: [[OFFSET1:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 1
    // CHECK-NEXT: store i8 3, i8* [[OFFSET1]], align 1
    //
    // First argument bytes.
    //
    // CHECK-NEXT: [[OFFSET2:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 2
    // CHECK-NEXT: store i8 0, i8* [[OFFSET2]], align 1
    // CHECK-NEXT: [[OFFSET3:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 3
    // CHECK-NEXT: store i8 4, i8* [[OFFSET3]], align 1
    // CHECK-NEXT: [[OFFSET4:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 4
    // CHECK-NEXT: [[BITCASTED:%.+]] = bitcast i8* [[OFFSET4]] to i32*
    // CHECK-NEXT: store i32 5, i32* [[BITCASTED]], align 1
    //
    // Second argument
    //
    // CHECK-NEXT: [[OFFSET12:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 8
    // CHECK-NEXT: store i8 16, i8* [[OFFSET12]], align 1
    // CHECK-NEXT: [[OFFSET13:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 9
    // CHECK-NEXT: store i8 4, i8* [[OFFSET13]], align 1
    // CHECK-NEXT: [[OFFSET14:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 10
    // CHECK-NEXT: [[BITCASTED2:%.+]] = bitcast i8* [[OFFSET14]] to i32*
    // CHECK-NEXT: store i32 10, i32* [[BITCASTED2]], align 1
    //
    // Third argument
    //
    // CHECK-NEXT: [[OFFSET22:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 14
    // CHECK-NEXT: store i8 0, i8* [[OFFSET22]], align 1
    // CHECK-NEXT: [[OFFSET23:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 15
    // CHECK-NEXT: store i8 4, i8* [[OFFSET23]], align 1
    // CHECK-NEXT: [[OFFSET24:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 16
    // CHECK-NEXT: [[BITCASTED3:%.+]] = bitcast i8* [[OFFSET24]] to i32*
    // CHECK-NEXT: store i32 2147483647, i32* [[BITCASTED3]], align 1
    //
    // os_log_impl call.
    // CHECK-NEXT: tail call swiftcc void @"${{.*}}_os_log_impl_test{{.*}}"({{.*}}, {{.*}}, {{.*}}, {{.*}}, i8* {{(nonnull )?}}getelementptr inbounds ([28 x i8], [28 x i8]* @{{.*}}, i{{.*}} 0, i{{.*}} 0), i8* {{(nonnull )?}}[[BUFFER]], i32 20)
    // CHECK-NEXT: tail call void @swift_slowDealloc(i8* {{(nonnull )?}}[[BUFFER]]
    // CHECK-NEXT: br label %[[NOT_ENABLED]]

    // CHECK: [[NOT_ENABLED]]:
    // CHECK-NEXT: tail call void @swift_release
    // CHECK-NEXT: ret void
}

// CHECK-LABEL: define hidden swiftcc void @"${{.*}}testStringInterpolation
func testStringInterpolation(stringValue: String) {
  _osLogTestHelper("String value: \(stringValue)")
    // CHECK: entry:
    // CHECK-64: call %swift.bridge* @swift_bridgeObjectRetain_n(%swift.bridge* %1
    // CHECK-32: tail call void @"$ss13_StringObjectV7VariantOWOy"(i32 %1
    // CHECK-32-NEXT: tail call void @"$ss13_StringObjectV7VariantOWOy"(i32 %1
    // CHECK: tail call swiftcc i1 @"${{.*}}isLoggingEnabled{{.*}}"()
    // CHECK-NEXT: br i1 {{%.*}}, label %[[ENABLED:[0-9]+]], label %[[NOT_ENABLED:[0-9]+]]

    // CHECK: [[NOT_ENABLED]]:
    // CHECK-64: call void @swift_bridgeObjectRelease_n(%swift.bridge* %1
    // CHECK-32: tail call void @"$ss13_StringObjectV7VariantOWOe"(i32 %1
    // CHECK-32-NEXT: tail call void @"$ss13_StringObjectV7VariantOWOe"(i32 %1
    // CHECK: br label %[[EXIT:[0-9]+]]

    // CHECK: [[ENABLED]]:
    //
    // Header bytes.
    //
    // CHECK-64: [[BUFFER:%.+]] = tail call noalias i8* @swift_slowAlloc(i{{.*}} 12
    // CHECK-32: [[BUFFER:%.+]] = tail call noalias i8* @swift_slowAlloc(i{{.*}} 8
    // CHECK-64-NEXT: [[STR_STORAGE:%.+]] = tail call noalias i8* @swift_slowAlloc(i{{.*}} 32
    // CHECK-32-NEXT: [[STR_STORAGE:%.+]] = tail call noalias i8* @swift_slowAlloc(i{{.*}} 16
    // CHECK: call void @llvm.lifetime.start{{.*}}({{.*}}, i8* {{(nonnull )?}}[[STR_STORAGE_PTR:%.*]]
    // CHECK: store i8 2, i8* [[BUFFER]], align 1
    // CHECK-NEXT: [[OFFSET1:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 1
    // CHECK-NEXT: store i8 1, i8* [[OFFSET1]], align 1

    // Argument bytes.
    //
    // CHECK-NEXT: [[OFFSET2:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 2
    // CHECK-NEXT: store i8 32, i8* [[OFFSET2]], align 1
    // CHECK-NEXT: [[OFFSET3:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 3
    // CHECK-64-NEXT: store i8 8, i8* [[OFFSET3]], align 1
    // CHECK-32-NEXT: store i8 4, i8* [[OFFSET3]], align 1
    
    // CHECK: [[STR_POINTER:%.*]] = call swiftcc i8* @"${{.*}}getNullTerminatedUTF8Pointer{{.*}}"(i{{.*}} %0, {{.*}} %1

    // CHECK: [[OFFSET_BUFFER:%.*]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 4
    // CHECK-NEXT: [[OFFSET_BUFFER_PTR:%.*]] = bitcast i8* [[OFFSET_BUFFER]] to i8**
    // CHECK-NEXT: store i8* [[STR_POINTER]], i8** [[OFFSET_BUFFER_PTR]]

    // os_log_impl call.
    // CHECK-64: tail call swiftcc void @"${{.*}}_os_log_impl_test{{.*}}"({{.*}}, {{.*}}, {{.*}}, {{.*}}, i8* {{(nonnull )?}}getelementptr inbounds ([17 x i8], [17 x i8]* @{{.*}}, i64 0, i64 0), i8* {{(nonnull )?}}[[BUFFER]], i32 12)
    // CHECK-32: tail call swiftcc void @"${{.*}}_os_log_impl_test{{.*}}"({{.*}}, {{.*}}, {{.*}}, {{.*}}, i8* {{(nonnull )?}}getelementptr inbounds ([17 x i8], [17 x i8]* @{{.*}}, i32 0, i32 0), i8* {{(nonnull )?}}[[BUFFER]], i32 8)
    // CHECK-NEXT: [[BITCASTED_STR_STORAGE:%.*]] = bitcast i8* [[STR_STORAGE]] to %swift.opaque*
    // CHECK-NEXT: tail call void @swift_arrayDestroy(%swift.opaque* [[BITCASTED_STR_STORAGE]]
    // CHECK-NEXT: tail call void @swift_slowDealloc(i8* {{(nonnull )?}}[[STR_STORAGE]]
    // CHECK-NEXT: tail call void @swift_slowDealloc(i8* {{(nonnull )?}}[[BUFFER]]
    // CHECK: call void @llvm.lifetime.end{{.*}}({{.*}}, i8* {{(nonnull )?}}[[STR_STORAGE_PTR]]
    // CHECK-NEXT: br label %[[EXIT]]
  
    // CHECK: [[EXIT]]:
    // CHECK-NEXT: ret void
}

// CHECK-LABEL: define hidden swiftcc void @"${{.*}}testMetatypeInterpolation
func testMetatypeInterpolation<T>(of type: T.Type) {
    _osLogTestHelper("Metatype value: \(type)")
    // CHECK: entry:
    // CHECK: tail call swiftcc i1 @"${{.*}}isLoggingEnabled{{.*}}"()
    // CHECK-NEXT: br i1 {{%.*}}, label %[[ENABLED:[0-9]+]], label %[[NOT_ENABLED:[0-9]+]]

    // CHECK: [[NOT_ENABLED]]:
    // CHECK-NEXT: call void @swift_release
    // CHECK: br label %[[EXIT:[0-9]+]]

    // CHECK: [[ENABLED]]:
    //
    // Header bytes.
    //
    // CHECK-64: [[BUFFER:%.+]] = tail call noalias i8* @swift_slowAlloc(i{{.*}} 12
    // CHECK-32: [[BUFFER:%.+]] = tail call noalias i8* @swift_slowAlloc(i{{.*}} 8
    // CHECK-64-NEXT: [[STR_STORAGE:%.+]] = tail call noalias i8* @swift_slowAlloc(i{{.*}} 32
    // CHECK-32-NEXT: [[STR_STORAGE:%.+]] = tail call noalias i8* @swift_slowAlloc(i{{.*}} 16
    // CHECK: call void @llvm.lifetime.start{{.*}}({{.*}}, i8* {{(nonnull )?}}[[STR_STORAGE_PTR:%.*]]
    // CHECK: store i8 2, i8* [[BUFFER]], align 1
    // CHECK-NEXT: [[OFFSET1:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 1
    // CHECK-NEXT: store i8 1, i8* [[OFFSET1]], align 1

    // Argument bytes.
    //
    // CHECK-NEXT: [[OFFSET2:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 2
    // CHECK-NEXT: store i8 32, i8* [[OFFSET2]], align 1
    // CHECK-NEXT: [[OFFSET3:%.+]] = getelementptr inbounds i8, i8* [[BUFFER]], i{{.*}} 3
    // CHECK-64-NEXT: store i8 8, i8* [[OFFSET3]], align 1
    // CHECK-32-NEXT: store i8 4, i8* [[OFFSET3]], align 1
    // CHECK-NEXT: [[TYPENAME:%.+]] = tail call swiftcc { i{{.*}}, {{.*}} } @"${{.*}}_typeName{{.*}}"({{.*}} %0
    // CHECK-NEXT: [[TYPENAME_0:%.+]] = extractvalue { i{{.*}}, {{.*}} } [[TYPENAME]], 0
    // CHECK-NEXT: [[TYPENAME_1:%.+]] = extractvalue { i{{.*}}, {{.*}} } [[TYPENAME]], 1
    
    // CHECK: [[STR_POINTER:%.*]] = call swiftcc i8* @"${{.*}}getNullTerminatedUTF8Pointer{{.*}}"(i{{.*}} [[TYPENAME_0]], {{.*}} [[TYPENAME_1]]

    // os_log_impl call.
    // CHECK-64: tail call swiftcc void @"${{.*}}_os_log_impl_test{{.*}}"({{.*}}, {{.*}}, {{.*}}, {{.*}}, i8* {{(nonnull )?}}getelementptr inbounds ([19 x i8], [19 x i8]* @{{.*}}, i64 0, i64 0), i8* {{(nonnull )?}}[[BUFFER]], i32 12)
    // CHECK-32: tail call swiftcc void @"${{.*}}_os_log_impl_test{{.*}}"({{.*}}, {{.*}}, {{.*}}, {{.*}}, i8* {{(nonnull )?}}getelementptr inbounds ([19 x i8], [19 x i8]* @{{.*}}, i32 0, i32 0), i8* {{(nonnull )?}}[[BUFFER]], i32 8)
    // CHECK-NEXT: [[BITCASTED_STR_STORAGE:%.*]] = bitcast i8* [[STR_STORAGE]] to %swift.opaque*
    // CHECK-NEXT: tail call void @swift_arrayDestroy(%swift.opaque* [[BITCASTED_STR_STORAGE]]
    // CHECK-NEXT: tail call void @swift_slowDealloc(i8* {{(nonnull )?}}[[STR_STORAGE]]
    // CHECK-NEXT: tail call void @swift_slowDealloc(i8* {{(nonnull )?}}[[BUFFER]]
    // CHECK: call void @llvm.lifetime.end{{.*}}({{.*}}, i8* {{(nonnull )?}}[[STR_STORAGE_PTR]]
    // CHECK-NEXT: br label %[[EXIT]]
  
    // CHECK: [[EXIT]]:
    // CHECK-NEXT: ret void
}
