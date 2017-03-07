protocol P1 {}

struct S1 : P1 {}

func test(_ o: P1?) {
  switch o {
  case let s as S1:
    test(s)
  }
}

// RUN: %sourcekitd-test -req=cursor -pos=7:17 %s -- %s | %FileCheck -check-prefix=CHECK1 %s
// CHECK1: source.lang.swift.ref.struct (3:8-3:10)
// CHECK1: S1
