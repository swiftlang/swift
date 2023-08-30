
class C1 {
  init(passInt: Int, andThis: Float) {}
  func meth(_ x: Int, passFloat: Float) {}
}
var c1 = C1(passInt: 0, andThis: 0)
c1.meth(0, passFloat: 0)

// RUN: %sourcekitd-test -req=cursor -pos=6:11 %s -- %s | %FileCheck -check-prefix=CHECK-CLASS %s
// CHECK-CLASS: source.lang.swift.ref.class (2:7-2:9)
// CHECK-CLASS: s:15cursor_callargs2C1C
// CHECK-CLASS: SECONDARY SYMBOLS BEGIN
// CHECK-CLASS-NEXT: source.lang.swift.ref.function.constructor {{[^|]*}}
// CHECK-CLASS-NEXT: init(passInt:andThis:)
// CHECK-CLASS-NEXT: s:15cursor_callargs2C1C7passInt7andThisACSi_Sftcfc
// CHECK-CLASS-NEXT: source.lang.swift
// CHECK-CLASS: -----
// CHECK-CLASS-NEXT: SECONDARY SYMBOLS END

// RUN: %sourcekitd-test -req=cursor -pos=6:17 %s -- %s | %FileCheck -check-prefix=CHECK-PAR-ONE %s
// RUN: %sourcekitd-test -req=cursor -pos=6:31 %s -- %s | %FileCheck -check-prefix=CHECK-PAR-TWO %s
// CHECK-PAR-ONE: source.lang.swift.ref.var.local (3:8-3:15)
// CHECK-PAR-TWO: source.lang.swift.ref.var.local (3:22-3:29)

// RUN: %sourcekitd-test -req=cursor -pos=7:6 %s -- %s | %FileCheck -check-prefix=CHECK-METH %s
// RUN: %sourcekitd-test -req=cursor -pos=7:14 %s -- %s | %FileCheck -check-prefix=CHECK-PAR %s
// CHECK-METH: source.lang.swift.ref.function.method.instance (4:8-4:40)
// CHECK-PAR: source.lang.swift.ref.var.local (4:23-4:32)

// Make sure we don't highlight all "meth" occurrences when pointing at "withFloat:".
// RUN: %sourcekitd-test -req=related-idents -pos=7:15 %s -- %s | %FileCheck -check-prefix=CHECK-IDS %s
// CHECK-IDS:      START RANGES
// CHECK-IDS-NEXT: END RANGES
