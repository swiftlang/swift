// Batch-mode has no good place to emit diagnostics that occur in non-primary files
// because then cannot be locatlized to a particular primary's serialized diagnostics file.
// When such an error occurs, a nonspecific error must be emitted
// for the primary files so Xcode knows something happened.
//
// RUN: rm -f %t.*

// RUN: not %target-swift-frontend -typecheck  -primary-file %s  -serialize-diagnostics-path %t.main.dia -primary-file %S/../Inputs/empty.swift  -serialize-diagnostics-path %t.empty.dia %S/Inputs/serialized-diagnostics-batch-mode-suppression-helper.swift  2> %t.stderr.txt
// RUN: c-index-test -read-diagnostics %t.main.dia 2> %t.main.txt
// RUN: c-index-test -read-diagnostics %t.empty.dia 2> %t.empty.txt

// RUN: %FileCheck -check-prefix=NO-ERROR_OCCURRED %s <%t.main.txt
// RUN: %FileCheck -check-prefix=ERROR_OCCURRED %s <%t.empty.txt
// ERROR_OCCURRED: an error occurred
// NO-ERROR_OCCURRED-NOT: an error occurred


func test(x: SomeType) {
  nonexistant(); // create an error here
}
