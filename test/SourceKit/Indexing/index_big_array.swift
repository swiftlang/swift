// RUN: %sourcekitd-test -req=index %S/../Inputs/big_array.swift -- %S/../Inputs/big_array.swift -Xfrontend -disable-implicit-concurrency-module-import | %sed_clean > %t.response
// RUN: %diff -u %s.response %t.response
