// RUN: %sourcekitd-test -req=doc-info %S/Inputs/main.swift > %t.response
// RUN: diff -u %s.response %t.response
