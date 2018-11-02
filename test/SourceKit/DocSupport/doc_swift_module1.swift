// RUN: %empty-directory(%t.mod)
// RUN: %swift -emit-module -o %t.mod/cake1.swiftmodule %S/Inputs/cake1.swift -parse-as-library
// RUN: %sourcekitd-test -req=doc-info -module cake1 -- -I %t.mod > %t.response
// RUN: diff -u %s.response %t.response
