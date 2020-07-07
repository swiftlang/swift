func foo() {}
foo()

// RUN: %sourcekitd-test -req=open %s -- %s \
// RUN:    == -req=edit -offset=0 -replace="" -length=0 %s \
// RUN:    == -req=edit -offset=0 -replace="" -length=0 %s \
// RUN:    == -req=edit -pos=3:1 -replace="foo()" -length=0 %s \
// RUN:    == -req=edit -offset=0 -replace="" -length=0 %s \
// RUN:    == -req=edit -offset=0 -replace="" -length=0 %s > %t.response
// RUN: %diff -u %s.response %t.response
