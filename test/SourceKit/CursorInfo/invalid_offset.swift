let a = 12

// rdar://problem/30346106
// Invalid offset should not trigger a crash.

// RUN: not %sourcekitd-test 2>&1 \
// RUN:   -req=open %s -- %s == \
// RUN:   -req=edit -async -offset=0 -length=200 -replace='' %s -- %s == \
// RUN:   -req=cursor -offset=250 %s -- %s

// rdar://problem/38162017
// REQUIRES: OS=macosx

// FIXME: this test is failing (rarely) in CI
// REQUIRES: rdar71468441
