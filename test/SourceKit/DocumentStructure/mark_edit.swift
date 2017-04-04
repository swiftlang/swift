// XFAIL: linux
// RUN: %sourcekitd-test -req=structure -pos=1:1 -length=0 -replace=" " %S/Inputs/mark.swift > %t.response
// RUN: diff -u %s.response %t.response
