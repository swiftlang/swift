// The shape of a log: tool, one artifact per file, a rule per diagnostic kind,
// and a result per diagnostic.

// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -typecheck -serialize-diagnostics=sarif -serialize-diagnostics-path %t/diags.sarif %s
// RUN: %normalize_sarif %t/diags.sarif | %diff -U1 -b %S/Inputs/expected-sarif/sarif-diagnostics.sarif -

// REQUIRES: swift_sarif

// An error, its note, and an unrelated warning. Notes are separate results for
// now, rather than related locations.
struct Point { var x: Int; var y: Int }
let p = Point()
func f() { let unused = 1 }
