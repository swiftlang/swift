struct Person {
    var name:String
    init(aName:String) {
        self.name = aName
    }

    subscript(x: Int) -> Int {
      return x
    }
}

// rdar://24133008
// RUN: %sourcekitd-test -req=cursor -pos=4:16 %s -- %s | %FileCheck %s -check-prefix=CASE1
// RUN: %sourcekitd-test -req=cursor -pos=4:24 %s -- %s | %FileCheck %s -check-prefix=CASE2
// RUN: %sourcekitd-test -req=cursor -pos=7:15 %s -- %s | %FileCheck %s -check-prefix=CASE3

// CASE1: source.lang.swift.ref.var.instance (2:9-2:13)
// CASE2: source.lang.swift.ref.var.local (3:10-3:15)
// CASE3: source.lang.swift.decl.var.parameter (7:15-7:16)
