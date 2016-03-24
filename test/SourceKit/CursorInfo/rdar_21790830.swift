extension Undeclared {
  func foo() {}
}

// RUN: %sourcekitd-test -req=cursor -pos=2:8 %s -- %s
// CHECK: <Declaration>func foo()
