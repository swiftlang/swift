struct Foo {
  static func foo(a: () -> Int) {}
  func qux(x: Int, y: () -> Int ) {}
}

func testTrailingClosure() -> String {
  Foo.foo(a: { 1 })
  Foo.bar(a: { print(3); return 1 })
  Foo().qux(x: 1, y: { 1 })
  let _ = Foo().quux(x: 1, y: { 1 })

  [1,2,3]
    .filter({ $0 % 2 == 0 })
    .map({ $0 + 1 })

  Foo.foo { 1 }
  Foo.bar { print(3); return 1 }
  Foo().qux(x: 1) { 1 }
}

// RUN: %refactor -source-filename %s -pos=7:3 | %FileCheck %s -check-prefix=CHECK-TRAILING-CLOSURE
// RUN: %refactor -source-filename %s -pos=7:6 | %FileCheck %s -check-prefix=CHECK-TRAILING-CLOSURE
// RUN: %refactor -source-filename %s -pos=7:7 | %FileCheck %s -check-prefix=CHECK-TRAILING-CLOSURE
// RUN: %refactor -source-filename %s -pos=7:10 | %FileCheck %s -check-prefix=CHECK-TRAILING-CLOSURE
// RUN: %refactor -source-filename %s -pos=7:11 | %FileCheck %s -check-prefix=CHECK-TRAILING-CLOSURE
// RUN: %refactor -source-filename %s -pos=7:12 | %FileCheck %s -check-prefix=CHECK-TRAILING-CLOSURE
// RUN: %refactor -source-filename %s -pos=7:14 | %FileCheck %s -check-prefix=CHECK-TRAILING-CLOSURE
// RUN: %refactor -source-filename %s -pos=7:16 | %FileCheck %s -check-prefix=CHECK-NO-TRAILING-CLOSURE
// RUN: %refactor -source-filename %s -pos=7:18 | %FileCheck %s -check-prefix=CHECK-TRAILING-CLOSURE
// RUN: %refactor -source-filename %s -pos=7:19 | %FileCheck %s -check-prefix=CHECK-TRAILING-CLOSURE

// RUN: %refactor -source-filename %s -pos=8:3 | %FileCheck %s -check-prefix=CHECK-TRAILING-CLOSURE
// RUN: %refactor -source-filename %s -pos=8:11 | %FileCheck %s -check-prefix=CHECK-TRAILING-CLOSURE

// RUN: %refactor -source-filename %s -pos=9:3 | %FileCheck %s -check-prefix=CHECK-NO-TRAILING-CLOSURE
// RUN: %refactor -source-filename %s -pos=9:8 | %FileCheck %s -check-prefix=CHECK-TRAILING-CLOSURE

// RUN: %refactor -source-filename %s -pos=10:3 | %FileCheck %s -check-prefix=CHECK-NO-TRAILING-CLOSURE
// RUN: %refactor -source-filename %s -pos=10:9 | %FileCheck %s -check-prefix=CHECK-NO-TRAILING-CLOSURE
// RUN: %refactor -source-filename %s -pos=10:17 | %FileCheck %s -check-prefix=CHECK-TRAILING-CLOSURE

// RUN: %refactor -source-filename %s -pos=12:4 | %FileCheck %s -check-prefix=CHECK-NO-TRAILING-CLOSURE
// RUN: %refactor -source-filename %s -pos=13:5 | %FileCheck %s -check-prefix=CHECK-TRAILING-CLOSURE
// RUN: %refactor -source-filename %s -pos=14:5 | %FileCheck %s -check-prefix=CHECK-TRAILING-CLOSURE

// RUN: %refactor -source-filename %s -pos=16:7 | %FileCheck %s -check-prefix=CHECK-NO-TRAILING-CLOSURE
// RUN: %refactor -source-filename %s -pos=17:7 | %FileCheck %s -check-prefix=CHECK-NO-TRAILING-CLOSURE
// RUN: %refactor -source-filename %s -pos=18:9 | %FileCheck %s -check-prefix=CHECK-NO-TRAILING-CLOSURE

// CHECK-TRAILING-CLOSURE: Convert To Trailing Closure

// CHECK-NO-TRAILING-CLOSURE: Action begins
// CHECK-NO-TRAILING-CLOSURE-NOT: Convert To Trailing Closure
// CHECK-NO-TRAILING-CLOSURE: Action ends
