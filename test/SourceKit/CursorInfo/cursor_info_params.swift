class C1 {
  init(cc cc: Int) {}
  func foo(aa aa : Int) {}
  subscript(aa aa : Int, bb bb: Int)-> Int { get { return 0 } set {}}
}

// RUN: %sourcekitd-test -req=cursor -pos=2:9 %s -- %s | %FileCheck %s -check-prefix=CHECK1
// RUN: %sourcekitd-test -req=cursor -pos=3:13 %s -- %s | %FileCheck %s -check-prefix=CHECK2
// RUN: %sourcekitd-test -req=cursor -pos=4:14 %s -- %s | %FileCheck %s -check-prefix=CHECK3
// RUN: %sourcekitd-test -req=cursor -pos=4:27 %s -- %s | %FileCheck %s -check-prefix=CHECK3

// CHECK1: source.lang.swift.decl.function.constructor
// CHECK2: source.lang.swift.decl.function.method.instance
// CHECK3: source.lang.swift.decl.function.subscript
