print("a")

// RUN: %sourcekitd-test -req=syntax-map %s == -req=cursor -pos=1:2 %s -- %s | %FileCheck -check-prefix=CHECK1 %s
// CHECK1: source.lang.swift.ref.function.free ()
// CHECK1-NEXT: print
