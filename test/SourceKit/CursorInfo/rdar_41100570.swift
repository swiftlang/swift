func foo(x: String) {
  defer { _ = x.count }
}

// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=2:15 %s -- %s | %FileCheck %s
// CHECK: source.lang.swift.ref.var.local (1:10-1:11)
