// RUN: %sourcekitd-test -req=structure %S/Inputs/main.swift -- -module-name StructureTest %S/Inputs/main.swift | %sed_clean > %t.response
// RUN: %diff -u %s.response %t.response

// RUN: %sourcekitd-test -req=structure %S/Inputs/invalid.swift | %sed_clean > %t.invalid.response
// RUN: %diff -u %s.invalid.response %t.invalid.response

// RUN: %sourcekitd-test -req=structure %S/../Inputs/placeholders.swift | %sed_clean > %t.placeholders.response
// RUN: %diff -u %s.placeholders.response %t.placeholders.response

// RUN: %sourcekitd-test -req=structure %S/Inputs/raw-identifiers.swift | %sed_clean > %t.raw-identifiers.swift.response
// RUN: %diff -u %s.raw-identifiers.swift.response %t.raw-identifiers.swift.response

// RUN: %sourcekitd-test -req=structure %S/Inputs/main.swift -name -foobar | %sed_clean > %t.foobar.response
// RUN: %diff -u %s.foobar.response %t.foobar.response

// RUN: %sourcekitd-test -req=structure -text-input %S/Inputs/main.swift | %sed_clean > %t.empty.response
// RUN: %diff -u %s.empty.response %t.empty.response
