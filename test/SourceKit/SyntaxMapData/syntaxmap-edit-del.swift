// RUN: %sourcekitd-test -req=syntax-map -pos=2:1 -length=2 -replace=" " %S/Inputs/syntaxmap-edit-del.swift | %sed_clean > %t.response
// RUN: diff -u %s.response %t.response
// RUN: %sourcekitd-test -req=syntax-map -pos=4:1 -length=2 -replace="" %S/Inputs/syntaxmap-edit-del.swift | %sed_clean > %t.response2
// RUN: diff -u %s.response2 %t.response2

// RUN: %sourcekitd-test -req=syntax-map -pos=2:1 -length=2 -replace=" " %S/Inputs/syntaxmap-edit-del.swift -force-libsyntax-based-processing | %sed_clean > %t.libsyntax.response
// RUN: diff -u %s.response %t.libsyntax.response
// RUN: %sourcekitd-test -req=syntax-map -pos=4:1 -length=2 -replace="" %S/Inputs/syntaxmap-edit-del.swift -force-libsyntax-based-processing | %sed_clean > %t.libsyntax.response2
// RUN: diff -u %s.response2 %t.libsyntax.response2
