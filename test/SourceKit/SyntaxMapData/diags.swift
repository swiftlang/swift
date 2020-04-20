// RUN: %sourcekitd-test -req=sema %S/Inputs/parse_error.swift -- %S/Inputs/parse_error.swift == \
// RUN:       -req=edit -pos=3:1 -length=0 -replace="   " %S/Inputs/parse_error.swift == -req=print-diags %S/Inputs/parse_error.swift | %sed_clean > %t.response
// RUN: %diff -u %s.response %t.response
