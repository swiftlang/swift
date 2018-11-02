class Foo {
  func foo() -> Int { return foo() }
  init() {}
  init(_ a: Int) {}
}
_ = Foo.init()
_ = Foo.init(2)

// RUN: %sourcekitd-test -req=cursor -pos=1:9 -cursor-action %s -- %s | %FileCheck -check-prefix=CHECK1 %s
// RUN: %sourcekitd-test -req=cursor -pos=2:9 %s -- %s | %FileCheck -check-prefix=CHECK2 %s
// RUN: %sourcekitd-test -req=cursor -pos=2:32 -cursor-action %s -- %s | %FileCheck -check-prefix=CHECK1 %s
// RUN: %sourcekitd-test -req=cursor -pos=2:18 %s -- %s | %FileCheck -check-prefix=CHECK2 %s
// RUN: %sourcekitd-test -req=cursor -pos=3:3 -cursor-action %s -- %s | %FileCheck -check-prefix=CHECK2 %s
// RUN: %sourcekitd-test -req=cursor -pos=4:3 -cursor-action %s -- %s | %FileCheck -check-prefix=CHECK1 %s
// RUN: %sourcekitd-test -req=cursor -pos=6:9 -cursor-action %s -- %s | %FileCheck -check-prefix=CHECK2 %s
// RUN: %sourcekitd-test -req=cursor -pos=7:9 -cursor-action %s -- %s | %FileCheck -check-prefix=CHECK1 %s

// CHECK1: Rename
// CHECK2-NOT: Rename

// REQUIRES-ANY: OS=macosx, OS=linux-gnu