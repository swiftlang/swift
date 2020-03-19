// RUN: %sourcekitd-test -req=syntax-map -pos=4:10 -replace="Bar" %S/Inputs/syntaxmap-edit.swift > %t.response
// RUN: diff --strip-trailing-cr -u %s.response %t.response
