// RUN: %target-swift-frontend -module-name=A -O -emit-ir %s | %FileCheck %s

// LLVM uses the assume intrinsic to strength reduce the division.

// CHECK-LABEL: define swiftcc i64 @"$s1A10testAssumeyS2iF"(i64)
// CHECK:   [[COND:%.*]] = icmp sgt i64 %0, -1
// CHECK:   tail call void @llvm.assume(i1 [[COND]])
// CHECK:   [[RES:%.*]] = lshr i64 %0, 6
// CHECK:   ret i64 [[RES]]

public func testAssume(_ i: Int) -> Int {
  let cond = i >= 0
  _assume(cond)
  return i / 64
}
