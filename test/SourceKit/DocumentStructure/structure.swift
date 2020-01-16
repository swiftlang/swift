// RUN: %sourcekitd-test -req=structure %S/Inputs/main.swift -- -module-name StructureTest %S/Inputs/main.swift | %sed_clean > %t.response
// RUN: diff --strip-trailing-cr -u %s.response %t.response

// RUN: %sourcekitd-test -req=structure %S/Inputs/invalid.swift | %sed_clean > %t.invalid.response
// RUN: diff --strip-trailing-cr -u %s.invalid.response %t.invalid.response

// RUN: %sourcekitd-test -req=structure %S/../Inputs/placeholders.swift | %sed_clean > %t.placeholders.response
// RUN: diff --strip-trailing-cr -u %s.placeholders.response %t.placeholders.response

// RUN: %sourcekitd-test -req=structure %S/Inputs/main.swift -name -foobar | %sed_clean > %t.foobar.response
// RUN: diff --strip-trailing-cr -u %s.foobar.response %t.foobar.response

// RUN: %sourcekitd-test -req=structure -text-input %S/Inputs/main.swift | %sed_clean > %t.empty.response
// RUN: diff --strip-trailing-cr -u %s.empty.response %t.empty.response
