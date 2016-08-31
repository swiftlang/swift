var t1 : Bool { return true }
var t2 : Bool { get { return true } }
var t3 : Bool { get { return true } set {} }

struct S1 {
  subscript(i: Int) -> Bool { return true }
}

// Checks that we don't crash.
// RUN: %sourcekitd-test -req=cursor -pos=1:15 %s -- %s | %FileCheck %s
// RUN: %sourcekitd-test -req=cursor -pos=1:17 %s -- %s | %FileCheck %s
// RUN: %sourcekitd-test -req=cursor -pos=2:15 %s -- %s | %FileCheck %s
// RUN: %sourcekitd-test -req=cursor -pos=2:17 %s -- %s | %FileCheck %s
// RUN: %sourcekitd-test -req=cursor -pos=2:21 %s -- %s | %FileCheck %s
// RUN: %sourcekitd-test -req=cursor -pos=2:23 %s -- %s | %FileCheck %s
// RUN: %sourcekitd-test -req=cursor -pos=3:15 %s -- %s | %FileCheck %s
// RUN: %sourcekitd-test -req=cursor -pos=3:17 %s -- %s | %FileCheck %s
// RUN: %sourcekitd-test -req=cursor -pos=3:21 %s -- %s | %FileCheck %s
// RUN: %sourcekitd-test -req=cursor -pos=3:23 %s -- %s | %FileCheck %s
// RUN: %sourcekitd-test -req=cursor -pos=3:37 %s -- %s | %FileCheck %s
// RUN: %sourcekitd-test -req=cursor -pos=3:41 %s -- %s | %FileCheck %s
// RUN: %sourcekitd-test -req=cursor -pos=6:29 %s -- %s | %FileCheck %s
// RUN: %sourcekitd-test -req=cursor -pos=6:31 %s -- %s | %FileCheck %s
// CHECK: <empty cursor info>
