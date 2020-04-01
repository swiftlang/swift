// RUN: %sourcekitd-test -req=index %S/../Inputs/big_array.swift -- %S/../Inputs/big_array.swift | %sed_clean > %t.response
// RUN: diff --strip-trailing-cr -u %s.response %t.response
