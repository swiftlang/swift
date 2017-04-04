// XFAIL: linux
// RUN: %sourcekitd-test -req=structure %S/Inputs/main.swift -- -module-name StructureTest %S/Inputs/main.swift | %sed_clean > %t.response
// RUN: diff -u %s.response %t.response

// RUN: %sourcekitd-test -req=structure %S/Inputs/invalid.swift | %sed_clean > %t.invalid.response
// RUN: diff -u %s.invalid.response %t.invalid.response

// RUN: %sourcekitd-test -req=structure %S/../Inputs/placeholders.swift | %sed_clean > %t.placeholders.response
// RUN: diff -u %s.placeholders.response %t.placeholders.response
