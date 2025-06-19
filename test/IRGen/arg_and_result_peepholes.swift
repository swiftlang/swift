// RUN: %target-swift-frontend %s  -import-objc-header %S/Inputs/arg_and_result_peepholes.h -emit-ir 2>&1 | %FileCheck %s

// REQUIRES: PTRSIZE=64

// CHECK: define{{.*}} swiftcc { i64, i64, i64, i64 } @"$s24arg_and_result_peepholes05test_D0ySo9BigStructVADF"(i64 %0, i64 %1, i64 %2, i64 %3)
// CHECK: entry:
// CHECK:   [[TMP:%.*]] = alloca { i64, i64, i64, i64 }
// CHECK:   [[RES_MEM:%.*]] = alloca %TSo9BigStructV
// CHECK:   [[ARG_MEM:%.*]] = alloca %TSo9BigStructV
// CHECK:   [[TMP2:%.*]] = alloca %TSo9BigStructV
// CHECK:   [[CALL:%.*]] = alloca %TSo9BigStructV
// CHECK:   call void @llvm.lifetime.start.p0(i64 256, ptr [[TMP]])
// CHECK:   [[A1:%.*]] = getelementptr inbounds{{.*}} { i64, i64, i64, i64 }, ptr [[TMP]], i32 0, i32 0
// CHECK:   store i64 %0, ptr [[A1]]
// CHECK:   [[A2:%.*]] = getelementptr inbounds{{.*}} { i64, i64, i64, i64 }, ptr [[TMP]], i32 0, i32 1
// CHECK:   store i64 %1, ptr [[A2]]
// CHECK:   [[A3:%.*]] = getelementptr inbounds{{.*}} { i64, i64, i64, i64 }, ptr [[TMP]], i32 0, i32 2
// CHECK:   store i64 %2, ptr [[A3]]
// CHECK:   [[A4:%.*]] = getelementptr inbounds{{.*}} { i64, i64, i64, i64 }, ptr [[TMP]], i32 0, i32 3
// CHECK:   store i64 %3, ptr [[A4]]

// CHECK:  call void @llvm.lifetime.start.p0(i64 32, ptr [[ARG_MEM]])
// CHECK:  call void @llvm.memcpy.p0.p0.i64(ptr{{.*}} [[ARG_MEM]], ptr{{.*}} [[TMP]], i64 32, i1 false)

// CHECK:  call void @llvm.lifetime.start.p0(i64 32, ptr [[TMP2]])
// CHECK:  call void @llvm.memcpy.p0.p0.i64(ptr{{.*}} [[TMP2]], ptr{{.*}} [[ARG_MEM]], i64 32, i1 false)
// CHECK:  call void @llvm.lifetime.start.p0(i64 32, ptr [[CALL]])
// CHECK:  call void @useBigStruct(ptr{{.*}} [[CALL]], ptr{{.*}} [[TMP2]])
// CHECK:  call void @llvm.lifetime.end.p0(i64 32, ptr [[TMP2]])

// CHECK: call void @llvm.memcpy.p0.p0.i64(ptr{{.*}} [[RES_MEM]], ptr{{.*}} [[CALL]], i64 32, i1 false)

// CHECK:  [[A5:%.*]] = getelementptr inbounds{{.*}} { i64, i64, i64, i64 }, ptr [[RES_MEM]], i32 0, i32 0
// CHECK:  [[R1:%.*]] = load i64, ptr [[A5]]
// CHECK:  [[A6:%.*]] = getelementptr inbounds{{.*}} { i64, i64, i64, i64 }, ptr [[RES_MEM]], i32 0, i32 1
// CHECK:  [[R2:%.*]] = load i64, ptr [[A6]]
// CHECK:  [[A7:%.*]] = getelementptr inbounds{{.*}} { i64, i64, i64, i64 }, ptr [[RES_MEM]], i32 0, i32 2
// CHECK:  [[R3:%.*]] = load i64, ptr [[A7]]
// CHECK:  [[A8:%.*]] = getelementptr inbounds{{.*}} { i64, i64, i64, i64 }, ptr [[RES_MEM]], i32 0, i32 3
// CHECK:  [[R4:%.*]] = load i64, ptr [[A8]]
// CHECK:  call void @llvm.lifetime.end.p0(i64 32, ptr [[ARG_MEM]])
// CHECK:  call void @llvm.lifetime.end.p0(i64 32, ptr [[RES_MEM]])

// CHECK:  [[R5:%.*]] = insertvalue { i64, i64, i64, i64 } undef, i64 [[R1]], 0
// CHECK:  [[R6:%.*]] = insertvalue { i64, i64, i64, i64 } [[R5]], i64 [[R2]], 1
// CHECK:  [[R7:%.*]] = insertvalue { i64, i64, i64, i64 } [[R6]], i64 [[R3]], 2
// CHECK:  [[R8:%.*]] = insertvalue { i64, i64, i64, i64 } [[R7]], i64 [[R4]], 3
// CHECK:  ret { i64, i64, i64, i64 } [[R8]]

public func test_peepholes(_ v: BigStruct) -> BigStruct {
  return useBigStruct(v)
}
