// XFAIL: linux
// RUN: %sourcekitd-test -req=structure -print-response-as-json %S/Inputs/main.swift > %t.response
// RUN: diff -u %s.response %t.response
