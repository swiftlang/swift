class C1 {
  init(cc: Int) {}
  func foo(aa : Int) {}
  subscript(aa : Int, bb: Int)-> Int { get { return 0 } set {}}
}
let c = C1(cc: 1)
c.foo(aa : 1)

// RUN: %sourcekitd-test -req=cursor -pos=2:9 %s -- %s | %FileCheck %s -check-prefix=CHECK1
// RUN: %sourcekitd-test -req=cursor -pos=3:13 %s -- %s | %FileCheck %s -check-prefix=CHECK2
// RUN: %sourcekitd-test -req=cursor -pos=4:24 %s -- %s | %FileCheck %s -check-prefix=CHECK3

// CHECK1: PARENT LOC: 2:3
// CHECK2: PARENT LOC: 3:8
// CHECK3: PARENT LOC: 4:3
