// Columns are UTF-16 code units, not UTF-8 bytes.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -serialize-diagnostics=sarif -serialize-diagnostics-path %t/diags.sarif %s
// RUN: %normalize_sarif %t/diags.sarif | %diff -U1 -b %S/Inputs/expected-sarif/sarif-diagnostics-columns.sarif -

// REQUIRES: swift_sarif

// 'é' is 2 bytes and 1 code unit; '😀' is 4 and 2.
func accented() { let café = 1; let unused = café }
func emoji() { let 😀 = 1; let unused = 😀 }
