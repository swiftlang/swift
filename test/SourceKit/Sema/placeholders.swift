// RUN: %sourcekitd-test -req=sema %S/../Inputs/placeholders.swift -- %S/../Inputs/placeholders.swift | %sed_clean > %t.placeholders.response
// RUN: %diff -u %s.placeholders.response %t.placeholders.response
