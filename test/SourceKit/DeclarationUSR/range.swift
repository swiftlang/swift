// RUN: %sourcekitd-test -req=collect-decl-usr -pos=5:8 -length=9 %s -- %s | %FileCheck %s -check-prefix=ONLY-FIRST
// RUN: %sourcekitd-test -req=collect-decl-usr -pos=6:1 -length=1 %s -- %s | %FileCheck %s -check-prefix=BODY-ONLY
// RUN: %sourcekitd-test -req=collect-decl-usr -pos=5:8 -length=58 %s -- %s | %FileCheck %s -check-prefix=FIRST-AND-SECOND

struct FirstName {
  let stored: Int
}

func secondName(parameter: Int) -> Int {
  return parameter
}

// ONLY-FIRST: key.kind: source.lang.swift.decl.struct
// ONLY-FIRST-NEXT: key.usr:
// ONLY-FIRST-SAME: FirstName
// ONLY-FIRST-NEXT: key.offset: 362
// ONLY-FIRST-NEXT: key.length: 9
// ONLY-FIRST-NOT: stored
// ONLY-FIRST-NOT: secondName

// BODY-ONLY: [
// BODY-ONLY-NEXT: ]

// FIRST-AND-SECOND: key.kind: source.lang.swift.decl.struct
// FIRST-AND-SECOND-NEXT: key.usr:
// FIRST-AND-SECOND-SAME: FirstName
// FIRST-AND-SECOND-NEXT: key.offset: 362
// FIRST-AND-SECOND-NEXT: key.length: 9
// FIRST-AND-SECOND: key.kind: source.lang.swift.decl.var.instance
// FIRST-AND-SECOND-NEXT: key.usr:
// FIRST-AND-SECOND-SAME: stored
// FIRST-AND-SECOND-NEXT: key.offset: 380
// FIRST-AND-SECOND-NEXT: key.length: 6
// FIRST-AND-SECOND: key.kind: source.lang.swift.decl.function.free
// FIRST-AND-SECOND-NEXT: key.usr:
// FIRST-AND-SECOND-SAME: secondName
// FIRST-AND-SECOND-NEXT: key.offset: 400
// FIRST-AND-SECOND-NEXT: key.length: 10
// FIRST-AND-SECOND: key.kind: source.lang.swift.decl.var.parameter
// FIRST-AND-SECOND-NEXT: key.usr:
// FIRST-AND-SECOND-SAME: parameter
// FIRST-AND-SECOND-NEXT: key.offset: 411
// FIRST-AND-SECOND-NEXT: key.length: 9
