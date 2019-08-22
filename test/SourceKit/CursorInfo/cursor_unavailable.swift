print("")
find([1,2,3],1)

// RUN: %sourcekitd-test -req=cursor -pos=1:1 %s -- %s | %FileCheck -check-prefix=CHECK1 %s
// RUN: %sourcekitd-test -req=cursor -pos=2:1 %s -- %s | %FileCheck -check-prefix=CHECK2 %s

// CHECK1: source.lang.swift.ref.function.free
// CHECK2: <empty cursor info; internal diagnostic: "Resolved to incomplete expression or statement.">
