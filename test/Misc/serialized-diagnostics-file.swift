// RUN: %empty-directory(%t)

// RUN: not %target-swift-frontend -typecheck -serialize-diagnostics-path %t/nonexistent/some.dia %s 2>%t.err.txt
// RUN: %FileCheck --input-file=%t.err.txt %s -check-prefix=OPEN-FAIL
// OPEN-FAIL: cannot open file '{{.*}}/nonexistent/some.dia' for diagnostics emission

var CRASH = 0

// Make sure no diagnostic file is created if the compiler crashes.
// RUN: not --crash %target-swift-frontend -typecheck -serialize-diagnostics-path %t/some.dia %s -debug-forbid-typecheck-prefix CRASH
// RUN: not find %t/some.dia
