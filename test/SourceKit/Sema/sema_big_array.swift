// RUN: %sourcekitd-test -req=sema %S/../Inputs/big_array.swift -- %S/../Inputs/big_array.swift > %t.response
// RUN: diff --strip-trailing-cr -u %s.response %t.response
