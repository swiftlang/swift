var $blah = 0

var x = 0
x

// RUN: %sourcekitd-test -req=sema %s -- %s -Xfrontend -playground | %sed_clean > %t1.response
// RUN: diff --strip-trailing-cr -u %s.response %t1.response
