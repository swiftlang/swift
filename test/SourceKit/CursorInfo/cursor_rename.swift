class Foo {
  func foo() -> Int { return foo() }
}

// RUN: %sourcekitd-test -req=cursor -pos=1:9 -cursor-action %s -- %s | %FileCheck -check-prefix=CHECK1 %s
// RUN: %sourcekitd-test -req=cursor -pos=2:9 %s -- %s | %FileCheck -check-prefix=CHECK2 %s
// RUN: %sourcekitd-test -req=cursor -pos=2:32 -cursor-action %s -- %s | %FileCheck -check-prefix=CHECK1 %s
// RUN: %sourcekitd-test -req=cursor -pos=2:18 %s -- %s | %FileCheck -check-prefix=CHECK2 %s

// CHECK1: Rename
// CHECK2-NOT: Rename

// REQUIRES-ANY: OS=macosx, OS=linux-gnu