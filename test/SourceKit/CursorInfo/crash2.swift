protocol P {
  func meth()
}

func foo (t : P) {
  t.meth()
}

// RUN: %sourcekitd-test -req=cursor -pos=6:5 %s -- %s | %FileCheck %s -check-prefix=CASE1

// CASE1: source.lang.swift.ref.function.method.instance (2:8-2:14)
