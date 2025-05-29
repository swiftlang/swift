func foo(_ x: Int) -> Int {}
func foo(_ x: String) -> Int {}

// rdar://131135631 - Make sure we can resolve solver-based cursor info in a
// VarDecl's accessor.
var x: Int {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):3 %s -- %s | %FileCheck %s
  foo()
}

// CHECK-DAG: source.lang.swift.ref.function.free (1:6-1:19)
// CHECK-DAG: source.lang.swift.ref.function.free (2:6-2:22)
