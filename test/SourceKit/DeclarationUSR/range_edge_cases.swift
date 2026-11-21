// RUN: %sourcekitd-test -req=collect-decl-usr -pos=9:10 -length=1 %s -- %s | %FileCheck %s -check-prefix=MID-TOKEN
// RUN: %sourcekitd-test -req=collect-decl-usr -pos=13:8 %s -- %s | %FileCheck %s -check-prefix=OFFSET-ONLY
// RUN: %sourcekitd-test -req=collect-decl-usr -pos=9:8 -req-opts=length=0 %s -- %s | %FileCheck %s -check-prefix=ZERO-LENGTH
// RUN: %sourcekitd-test -req=collect-decl-usr -pos=9:10 -req-opts=length=0 %s -- %s | %FileCheck %s -check-prefix=ZERO-LENGTH
// RUN: %sourcekitd-test -req=collect-decl-usr -req-opts=offset=4294967295 %s -- %s | %FileCheck %s -check-prefix=OUT-OF-FILE
// RUN: not %sourcekitd-test -req=collect-decl-usr -req-opts=offset=-1 -dont-print-request %s -- %s 2>&1 | %FileCheck %s -check-prefix=NEGATIVE-OFFSET
// RUN: not %sourcekitd-test -req=collect-decl-usr -req-opts=length=-1 -dont-print-request %s -- %s 2>&1 | %FileCheck %s -check-prefix=NEGATIVE-LENGTH

struct FirstName {
  let stored: Int
}

func secondName(parameter: Int) -> Int {
  return parameter
}

// MID-TOKEN: key.kind: source.lang.swift.decl.struct
// MID-TOKEN-NEXT: key.usr:
// MID-TOKEN-SAME: FirstName
// MID-TOKEN-NOT: stored
// MID-TOKEN-NOT: secondName

// OFFSET-ONLY-NOT: FirstName
// OFFSET-ONLY-NOT: stored
// OFFSET-ONLY: key.kind: source.lang.swift.decl.function.free
// OFFSET-ONLY-NEXT: key.usr:
// OFFSET-ONLY-SAME: secondName
// OFFSET-ONLY: key.kind: source.lang.swift.decl.var.parameter
// OFFSET-ONLY-NEXT: key.usr:
// OFFSET-ONLY-SAME: parameter

// ZERO-LENGTH: [
// ZERO-LENGTH-NEXT: ]

// OUT-OF-FILE: [
// OUT-OF-FILE-NEXT: ]

// NEGATIVE-OFFSET: error response (Request Invalid): 'key.offset' must be a non-negative 32-bit integer
// NEGATIVE-LENGTH: error response (Request Invalid): 'key.length' must be a non-negative 32-bit integer
