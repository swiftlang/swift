extension Any {
  public func foo() {
  }
}

// RUN: %sourcekitd-test -req=cursor -pos=2:15 %s -- %s | %FileCheck %s
// CHECK: source.lang.swift.decl.function.method.instance (2:15-2:20)
// CHECK: foo() 
