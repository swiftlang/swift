// RUN: %sourcekitd-test -req=doc-info %S/Inputs/main.swift > %t.response
// RUN: %diff -u %s.response %t.response

// RUN: not %sourcekitd-test -req=doc-info %S/Inputs/main.swift -- %S/Inputs/cake.swift 2> %t.error
// RUN: %FileCheck %s -check-prefix=MULTI_FILE < %t.error

// MULTI_FILE: unexpected input in compiler arguments
