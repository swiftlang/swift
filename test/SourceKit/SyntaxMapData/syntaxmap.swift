// XFAIL: broken_std_regex
// RUN: %sourcekitd-test -req=syntax-map %S/Inputs/syntaxmap.swift > %t.response
// RUN: diff -u %s.response %t.response