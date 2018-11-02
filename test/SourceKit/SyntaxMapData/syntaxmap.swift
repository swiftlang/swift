// XFAIL: broken_std_regex
// RUN: %sourcekitd-test -req=syntax-map %S/Inputs/syntaxmap.swift > %t.response
// RUN: diff -u %s.response %t.response
// RUN: %sourcekitd-test -req=syntax-map %S/Inputs/syntaxmap.swift -force-libsyntax-based-processing > %t.libsyntax.response
// RUN: diff -u %s.libsyntax.response %t.libsyntax.response
