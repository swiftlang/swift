// RUN: %empty-directory(%t)

// RUN: not %target-swift-frontend -typecheck -diagnostic-style llvm %s -sdk %t 2>&1 | %FileCheck -check-prefix=CHECK -check-prefix=NO-MODULE %s
// RUN: not %target-swift-frontend -typecheck -diagnostic-style llvm %s -resource-dir %t -sdk %t 2>&1 | %FileCheck -check-prefix=CHECK -check-prefix=NO-STDLIB %s

// NO-MODULE: error: no such module 'NonExistent'

// NO-STDLIB: error: unable to load standard library for target '{{.+-.+}}'

// CHECK-NOT: error
// CHECK-NOT: warning
import NonExistent

// No subsequent diagnostics after fatal errors.
var x: NonExistent.Something
