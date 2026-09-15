// Where a SARIF log is written, and what a clean compile produces.

// RUN: %empty-directory(%t)

// '=sarif' alone derives a '.sarif' path and writes no '.dia'.
// RUN: cp %s %t/derived.swift
// RUN: cd %t && %target-swift-frontend -typecheck -serialize-diagnostics=sarif derived.swift
// RUN: test ! -f %t/derived.dia

// No diagnostics still means a log, with no results and no artifacts.
// RUN: %normalize_sarif %t/derived.sarif | %diff -U1 -b %S/Inputs/expected-sarif/sarif-diagnostics-empty.sarif -

// A log that cannot be written is reported.
// RUN: not %target-swift-frontend -typecheck -serialize-diagnostics=sarif -serialize-diagnostics-path %t/nonexistent/some.sarif %s 2>%t.err.txt
// RUN: %FileCheck --input-file=%t.err.txt %s -check-prefix=OPEN-FAIL
// OPEN-FAIL: cannot open file '{{.*}}/nonexistent/some.sarif' for diagnostics emission

// REQUIRES: swift_sarif

let x = 1
