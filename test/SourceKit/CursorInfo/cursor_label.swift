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

class C2 {
  init(cc cc: Int) {}
  func foo(aa aa : Int) {}
  subscript(aa aa : Int, bb bb: Int)-> Int { get { return 0 } set {}}
}

// RUN: %sourcekitd-test -req=cursor -pos=2:9 %s -- %s | %FileCheck %s -check-prefix=CHECK1
// RUN: %sourcekitd-test -req=cursor -pos=3:13 %s -- %s | %FileCheck %s -check-prefix=CHECK2
// RUN: %sourcekitd-test -req=cursor -pos=4:24 %s -- %s | %FileCheck %s -check-prefix=CHECK3
// RUN: %sourcekitd-test -req=cursor -pos=5:15 %s -- %s | %FileCheck %s -check-prefix=CHECK-NO-PARENT-PARAM
// RUN: %sourcekitd-test -req=cursor -pos=6:11 %s -- %s | %FileCheck %s -check-prefix=CHECK-NO-PARENT-PARAM
// RUN: %sourcekitd-test -req=cursor -pos=7:16 %s -- %s | %FileCheck %s -check-prefix=CHECK-NO-PARENT-PARAM
// RUN: %sourcekitd-test -req=cursor -pos=8:18 %s -- %s | %FileCheck %s -check-prefix=CHECK-NO-PARENT-PARAM
// RUN: %sourcekitd-test -req=cursor -pos=8:28 %s -- %s | %FileCheck %s -check-prefix=CHECK4
// RUN: %sourcekitd-test -req=cursor -pos=9:9 %s -- %s | %FileCheck %s -check-prefix=CHECK-NO-PARENT-VAR
// RUN: %sourcekitd-test -req=cursor -pos=10:9 %s -- %s | %FileCheck %s -check-prefix=CHECK5

// CHECK1: source.lang.swift.decl.var.parameter
// CHECK1: PARENT OFFSET: 13

// CHECK2: source.lang.swift.decl.var.parameter
// CHECK2: PARENT OFFSET: 37

// CHECK3: source.lang.swift.decl.var.parameter
// CHECK3: PARENT OFFSET: 56

// CHECK4: source.lang.swift.decl.var.parameter
// CHECK4: PARENT OFFSET: 229

// CHECK5: source.lang.swift.ref.var.local
// CHECK5: PARENT OFFSET: 229

// CHECK-NO-PARENT-PARAM: source.lang.swift.decl.var.parameter
// CHECK-NO-PARENT-PARAM-NOT: PARENT OFFSET:

// CHECK-NO-PARENT-VAR: source.lang.swift.ref.var.local
// CHECK-NO-PARENT-VAR-NOT: PARENT OFFSET:

// RUN: %sourcekitd-test -req=cursor -pos=17:9 %s -- %s | %FileCheck %s -check-prefix=CHECK-CONSTRUCTOR
// RUN: %sourcekitd-test -req=cursor -pos=18:13 %s -- %s | %FileCheck %s -check-prefix=CHECK-FUNC
// RUN: %sourcekitd-test -req=cursor -pos=19:14 %s -- %s | %FileCheck %s -check-prefix=CHECK-SUBS
// RUN: %sourcekitd-test -req=cursor -pos=19:27 %s -- %s | %FileCheck %s -check-prefix=CHECK-SUBS

// CHECK-CONSTRUCTOR: source.lang.swift.decl.function.constructor
// CHECK-CONSTRUCTOR-NOT: PARENT OFFSET:

// CHECK-FUNC: source.lang.swift.decl.function.method.instance
// CHECK-FUNC-NOT: PARENT OFFSET:

// CHECK-SUBS: source.lang.swift.decl.function.subscript
// CHECK-SUBS-NOT: PARENT OFFSET:
