// RUN: %target-swift-frontend -O -Xllvm -swiftmergefunc-threshold=5 -module-name=test -emit-ir  %s | %FileCheck %s

// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O -module-name=test %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT

// REQUIRES: executable_test
// REQUIRES: CPU=arm64e

// CHECK: [[RELOC1:@[0-9]+]] = private constant { i8*, i32, i64, i64 } { i8* bitcast (void (i64)* @"$s4test7callee1yySiF" to i8*), i32 0, i64 0, i64 [[DISCR:[0-9]+]] }, section "llvm.ptrauth"
// CHECK: [[RELOC2:@[0-9]+]] = private constant { i8*, i32, i64, i64 } { i8* bitcast (void (i64)* @"$s4test7callee2yySiF" to i8*), i32 0, i64 0, i64 [[DISCR]] }, section "llvm.ptrauth"
@inline(never)
func callee1(_ i: Int) {
  print(i)
}

@inline(never)
func callee2(_ i: Int) {
  print("abc", i)
}

// CHECK-LABEL: define {{.*}} @"$s4test7testit1yyF"()
// CHECK: call {{.*}} @"$s4test7testit1yyFTm"(i64 27, void (i64)* bitcast ({ i8*, i32, i64, i64 }* [[RELOC1]] to void (i64)*))
@inline(never)
func testit1() {
  for _ in 0..<10 {
    callee1(27)
  }
}

// CHECK-LABEL: define {{.*}} @"$s4test7testit2yyF"()
// CHECK: call {{.*}} @"$s4test7testit1yyFTm"(i64 42, void (i64)* bitcast ({ i8*, i32, i64, i64 }* [[RELOC2]] to void (i64)*))
@inline(never)
public func testit2() {
  for _ in 0..<10 {
    callee2(42)
  }
}

// CHECK-LABEL: define {{.*}} @"$s4test7testit1yyFTm"(i64 %0, void (i64)* %1)
// CHECK: call {{.*}} %1(i64 %0) [ "ptrauth"(i32 0, i64 [[DISCR]]) ]

// CHECK-OUTPUT: 27
// CHECK-OUTPUT: 27
// CHECK-OUTPUT: 27
// CHECK-OUTPUT: 27
// CHECK-OUTPUT: 27
// CHECK-OUTPUT: 27
// CHECK-OUTPUT: 27
// CHECK-OUTPUT: 27
// CHECK-OUTPUT: 27
// CHECK-OUTPUT: 27
testit1()

// CHECK-OUTPUT: abc 42
// CHECK-OUTPUT: abc 42
// CHECK-OUTPUT: abc 42
// CHECK-OUTPUT: abc 42
// CHECK-OUTPUT: abc 42
// CHECK-OUTPUT: abc 42
// CHECK-OUTPUT: abc 42
// CHECK-OUTPUT: abc 42
// CHECK-OUTPUT: abc 42
// CHECK-OUTPUT: abc 42
testit2()
