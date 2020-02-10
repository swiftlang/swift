var x = 0

#if FOO
x = 1
#else
x = 2
#endif

// RUN: %sourcekitd-test -req=sema %s -- %s > %t.false.response
// RUN: diff --strip-trailing-cr -u %s.false.response %t.false.response
// RUN: %sourcekitd-test -req=sema %s -- %s -D FOO > %t.true.response
// RUN: diff --strip-trailing-cr -u %s.true.response %t.true.response
