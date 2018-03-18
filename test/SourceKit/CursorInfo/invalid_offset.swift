let a = 12

// rdar://problem/30346106
// Invalid offset should trigger a crash.

// RUN: %sourcekitd-test \
// RUN:   -req=open %s -- %s == \
// RUN:   -req=edit -async -offset=0 -length=200 -replace='' %s -- %s == \
// RUN:   -req=cursor -offset=250 %s -- %s \
// RUN: | %FileCheck %s

// rdar://problem/38162017
// REQUIRES: OS=macosx

// CHECK: <empty cursor info>
