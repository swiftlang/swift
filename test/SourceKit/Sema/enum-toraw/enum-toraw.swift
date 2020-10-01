// RUN: %sourcekitd-test -req=sema %S/Inputs/t2.swift -- %S/Inputs/t1.swift %S/Inputs/t2.swift | %sed_clean > %t.response
// RUN: %diff -u %s.response %t.response

