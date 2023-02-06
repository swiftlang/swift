func materialize() {
 let a = 2
}

func outer() {
  func inner() {
  }
}

// RUN: %sourcekitd-test -req=cursor -pos=2:6 %s -- %s == \
// RUN: -req=cursor -req-opts=retrieve_refactor_actions=1 -pos=6:8 %s -- %s | %FileCheck %s

// CHECK: source.lang.swift.decl.function.free (6:8-6:15)
// CHECK: inner()
// CHECK: ACTIONS BEGIN
// CHECK-DAG: source.refactoring.kind.rename.local
// CHECK-DAG: Local Rename
// CHECK-DAG: source.refactoring.kind.convert.func-to-async
// CHECK-DAG: Convert Function to Async
// CHECK: ACTIONS END
