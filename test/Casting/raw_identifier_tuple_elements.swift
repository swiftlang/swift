// RUN: %empty-directory(%t) 

// 1. functional test:

// RUN: %target-build-swift %s -emit-executable -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// 2. check if the generated IR looks like what we expect:

// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s -check-prefix=CHECK-IR

// REQUIRES: executable_test

func tupleCast<T, U>(_ t: (notRaw1: T, notRaw2: U)) -> Bool {
  return t is (notRaw1: Int, notRaw2: String)
  // CHECK-IR: @".str.16.notRaw1 notRaw2 " = private unnamed_addr constant [17 x i8] c"notRaw1 notRaw2 \00"
}

func tupleCast<T, U>(_ t: (`raw 1`: T, `raw 2`: U)) -> Bool {
  return t is (`raw 1`: Int, `raw 2`: String)
  // CHECK-IR: @".str.18.`raw\C2\A01` `raw\C2\A02` " = private unnamed_addr constant [19 x i8] c"`raw\C2\A01` `raw\C2\A02` \00"
}

func test() {
  var failed = false
  if !tupleCast((notRaw1: 10, notRaw2: "hello")) {
    print("failed: cast should have passed")
    failed = true
  }
  if tupleCast((notRaw1: 10, notRaw2: 11)) {
    print("failed: cast should not have passed")
    failed = true
  }

  if !tupleCast((`raw 1`: 10, `raw 2`: "hello")) {
    print("failed: cast should have passed")
    failed = true
  }
  if tupleCast((`raw 1`: 10, `raw 2`: 11)) {
    print("failed: cast should not have passed")
    failed = true
  }

  if !failed {
    print("passed")
  }
}

test()
// CHECK:     passed
// CHECK-NOT: failed
