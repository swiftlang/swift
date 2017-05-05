class C1 {
  init(cc: Int) {}
  func foo(aa : Int) {}
  subscript(aa : Int, bb: Int)-> Int { get { return 0 } set {}}
  func foo(_ aa : Int) {}
  init(_ cc: Int) {}
  subscript(_ aa : Int)-> Int { get { return 0 } set {}}
  func foo(label aa : Int, bb: Int) {
    _ = aa
    _ = bb
  }
}
let c = C1(cc: 1)
c.foo(aa : 1)

// RUN: %sourcekitd-test -req=cursor -pos=2:9 %s -- %s | %FileCheck %s -check-prefix=CHECK1
// RUN: %sourcekitd-test -req=cursor -pos=3:13 %s -- %s | %FileCheck %s -check-prefix=CHECK2
// RUN: %sourcekitd-test -req=cursor -pos=4:24 %s -- %s | %FileCheck %s -check-prefix=CHECK3
// RUN: %sourcekitd-test -req=cursor -pos=5:15 %s -- %s | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %sourcekitd-test -req=cursor -pos=6:11 %s -- %s | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %sourcekitd-test -req=cursor -pos=7:16 %s -- %s | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %sourcekitd-test -req=cursor -pos=8:18 %s -- %s | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %sourcekitd-test -req=cursor -pos=8:28 %s -- %s | %FileCheck %s -check-prefix=CHECK4
// RUN: %sourcekitd-test -req=cursor -pos=9:9 %s -- %s | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %sourcekitd-test -req=cursor -pos=10:9 %s -- %s | %FileCheck %s -check-prefix=CHECK4

// CHECK1: PARENT OFFSET: 13
// CHECK2: PARENT OFFSET: 37
// CHECK3: PARENT OFFSET: 56
// CHECK4: PARENT OFFSET: 229
// CHECK-NONE-NOT: PARENT OFFSET:
