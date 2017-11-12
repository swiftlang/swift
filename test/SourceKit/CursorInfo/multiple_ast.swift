// RUN: %sourcekitd-test -req=cursor -pos=1:8 %S/Inputs/multiple_ast1.swift -- %S/Inputs/multiple_ast1.swift == \
// RUN:       -req=cursor -pos=1:8 %S/Inputs/multiple_ast2.swift -- %S/Inputs/multiple_ast2.swift | %FileCheck -check-prefix=CHECK %s

// CHECK:      source.lang.swift.decl.function.free (1:6-1:20)
// CHECK-NEXT: foo1

// CHECK:      source.lang.swift.decl.function.free (1:6-1:20)
// CHECK-NEXT: foo2
