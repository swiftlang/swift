func foo(i: Int, complete: ()->()) {}

func main() {
  foo(i: 2) {
    print(2)
  }
}

func testIf(bool: Bool) {
  if bool {
    print(1)
  } else {
    print(1)
  }
}

func testDoCatch(canThrow: () throws -> Void) {
  do {
    try canThrow()
  } catch {
    print(error)
  }
}

func testSwitchCase(x: Int) {
  switch x {
    case 1:
      print(1)
    default:
      print(2)
  }
}


// RUN: %refactor -source-filename %s -pos=4:13 -end-pos=6:4 | %FileCheck %s -check-prefix=CHECK-NONE

// RUN: %refactor -source-filename %s -pos=10:11 -end-pos=12:4 | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %refactor -source-filename %s -pos=12:5 -end-pos=14:4 | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %refactor -source-filename %s -pos=12:10 -end-pos=14:4 | %FileCheck %s -check-prefix=CHECK-NONE

// RUN: %refactor -source-filename %s -pos=18:6 -end-pos=20:4 | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %refactor -source-filename %s -pos=20:5 -end-pos=22:4 | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %refactor -source-filename %s -pos=20:11 -end-pos=22:4 | %FileCheck %s -check-prefix=CHECK-NONE

// RUN: %refactor -source-filename %s -pos=26:12 -end-pos=31:4 | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %refactor -source-filename %s -pos=27:5 -end-pos=28:15 | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %refactor -source-filename %s -pos=29:5 -end-pos=30:15 | %FileCheck %s -check-prefix=CHECK-NONE

// CHECK-NONE: Action begins
// CHECK-NONE-NEXT: Action ends
