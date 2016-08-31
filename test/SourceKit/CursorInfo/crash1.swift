struct Person {
    var name:String
    init(aName:String) {
        self.name = aName
    }
}

// rdar://24133008
// RUN: %sourcekitd-test -req=cursor -pos=4:16 %s -- %s | %FileCheck %s -check-prefix=CASE1
// RUN: %sourcekitd-test -req=cursor -pos=4:24 %s -- %s | %FileCheck %s -check-prefix=CASE2

// CASE1: source.lang.swift.ref.var.instance (2:9-2:13)
// CASE2: source.lang.swift.ref.var.local (3:10-3:15)
