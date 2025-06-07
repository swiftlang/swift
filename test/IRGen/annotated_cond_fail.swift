// RUN: %target-swift-frontend -module-name A -enable-cond-fail-message-annotation -primary-file %s -O -emit-ir | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib
// REQUIRES: PTRSIZE=64

public func test(_ a: [Int], _ i: Int) -> Int {
    return a[i + 1]
}

// CHECK: define{{.*}} swiftcc i64 @"$s1A4testySiSaySiG_SitF"(ptr{{.*}} %0, i64 %1)

// CHECK:  [[ADD:%.*]] = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %1, i64 1)
// CHECK:  [[IDX:%.*]] = extractvalue { i64, i1 } [[ADD]], 0
// CHECK:  [[OVERFLOW:%.*]] = extractvalue { i64, i1 } [[ADD]], 1
// CHECK:  br i1 [[OVERFLOW]], {{.*}}!annotation ![[ARITH_OVERFLOW:[0-9]+]]

// CHECK:  [[C0:%.*]] = icmp slt i64 [[IDX]], 0
// CHECK:  br i1 [[C0]], {{.*}}!annotation ![[ARRAY_INDEX_OUT_OF_BOUNDS:[0-9]+]]

// CHECK:  [[SIZE_ADDR:%.*]] = getelementptr
// CHECK:  [[SIZE:%.*]] = load i64, ptr [[SIZE_ADDR]]
// CHECK:  [[C1:%.*]] = icmp ult i64 [[IDX]], [[SIZE]]
// CHECK:  br i1 [[C1]], {{.*}}!annotation ![[ARRAY_INDEX_OUT_OF_BOUNDS]]


// CHECK-DAG: ![[ARITH_OVERFLOW]] = !{!"arithmetic overflow"}
// CHECK-DAG: ![[ARRAY_INDEX_OUT_OF_BOUNDS]] = !{!"Index out of range"}
