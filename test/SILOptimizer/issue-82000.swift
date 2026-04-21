// RUN: %target-swift-frontend %s -emit-ir -O -solver-disable-crash-on-valid-salvage | %FileCheck %s

// REQUIRES: swift_in_compiler

// Missed optimization combining pointers and parameter packs #82000
func applyPointer<each T>(input: repeat UnsafePointer<each T>, op: (repeat each T) -> Int) -> Int {
  op(repeat (each input).pointee)
}

// TODO: This should constant-fold to 27, like testDirect below.
// CHECK: define {{.*}} i64 @"$s4main4testSiyF"()
// CHECK-NEXT: entry:
// CHECK-NEXT: [[OP1_LOC:%[0-9]+]] = alloca %TSi
// CHECK-NEXT: [[OP2_LOC:%[0-9]+]] = alloca %TSi
// CHECK-NEXT: [[IN_LOC:%[0-9]+]] = alloca %TSi
// CHECK:      store i64 27, ptr [[IN_LOC]], align 8
// CHECK:      %InitializeWithCopy.i(ptr noalias nonnull [[OP1_LOC]], ptr noalias nonnull [[IN_LOC]], ptr nonnull @"$sSiN")
// CHECK-NEXT:      %InitializeWithCopy.i(ptr noalias nonnull [[OP2_LOC]], ptr noalias nonnull [[IN_LOC]], ptr nonnull @"$sSiN")
// CHECK-NEXT: [[OP1:%[0-9]+]] = load {{.*}} [[OP1_LOC]]
// CHECK-NEXT: [[OP2:%[0-9]+]] = load {{.*}} [[OP2_LOC]]
// CHECK-NEXT: [[RESULT:%[0-9]+]] = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 [[OP1]], i64 [[OP2]])
public func test() -> Int {
    withUnsafePointer(to: 27) {
        applyPointer(input: $0, $0) { ($0 + $1) }
    }
}

func applyDirect<each T>(input: repeat each T, op: (repeat each T) -> Int) -> Int {
  op(repeat (each input))
}

// CHECK: define {{.*}} i64 @"$s4main10testDirectSiyF"()
// CHECK-NEXT: entry:
// CHECK-NEXT: ret i64 54
public func testDirect() -> Int {
    applyDirect(input: 27, 27) { ($0 + $1) }
}
