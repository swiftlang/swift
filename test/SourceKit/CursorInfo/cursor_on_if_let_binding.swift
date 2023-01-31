func foo() {
  let myVar: Int? = 2
  // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):10 %s -- %s | %FileCheck %s
  if let myVar = myVar {
  }
}

// CHECK: source.lang.swift.decl.var.local (4:10-4:15)
// CHECK-NEXT: myVar
// CHECK-NEXT: s:24cursor_on_if_let_binding3fooyyF5myVarL0_Sivp
// CHECK-NEXT: source.lang.swift
// CHECK-NEXT: Int
