// Batch-mode has no good place to emit diagnostics that occur in non-primary files
// because then cannot be locatlized to a particular primary's serialized diagnostics file.
// When such an error occurs, a nonspecific error must be emitted
// for the primary files so Xcode knows something happened.
//
// RUN: rm -f %t.*

// RUN: not %target-swift-frontend -typecheck  -primary-file %s  -serialize-diagnostics-path %t.main.dia -primary-file %S/../Inputs/empty.swift  -serialize-diagnostics-path %t.empty.dia %S/Inputs/serialized-diagnostics-batch-mode-suppression-helper.swift 2> %t.stderr.txt
// RUN: c-index-test -read-diagnostics %t.main.dia 2> %t.main.txt
// RUN: c-index-test -read-diagnostics %t.empty.dia 2> %t.empty.txt

// Ensure there was an error:

// RUN: %FileCheck -check-prefix=ERROR %s <%t.stderr.txt
// ERROR: error:

// Ensure the error is in the serialized diagnostics:

// RUN: %FileCheck -check-prefix=NO-NONSPECIFIC-ERROR %s <%t.main.txt
// RUN: %FileCheck -check-prefix=NONSPECIFIC-ERROR %s <%t.empty.txt
// NONSPECIFIC-ERROR: error: an error occurred
// NO-NONSPECIFIC-ERROR-NOT: error: an error occurred

func test(x: SomeType) {
  nonexistent()
}
