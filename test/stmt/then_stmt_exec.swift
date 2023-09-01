// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -Xfrontend -enable-experimental-feature -Xfrontend ThenStatements -o %t/a.out %s
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

// Required for experimental features
// REQUIRES: asserts

func testDefer(_ x: Bool) -> Int {
  defer {
    print("defer fn")
  }
  let x = if x {
    defer {
      print("defer if")
    }
    print("enter if")
    then 0
  } else {
    1
  }
  print("after if")
  return x
}
_ = testDefer(true)

// CHECK:      enter if
// CHECK-NEXT: defer if
// CHECK-NEXT: after if
// CHECK-NEXT: defer fn
