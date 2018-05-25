// Ensure that an error in a non-primary causes an error in the errorless primary.
//
// RUN: rm -f %t.*

// RUN: not %target-swift-frontend -typecheck  -primary-file %s  -serialize-diagnostics-path %t.main.dia -primary-file %S/../Inputs/empty.swift  -serialize-diagnostics-path %t.empty.dia %S/Inputs/serialized-diagnostics-batch-mode-suppression-helper.swift  2> %t.stderr.txt
// RUN: c-index-test -read-diagnostics %t.main.dia 2> %t.main.txt
// RUN: c-index-test -read-diagnostics %t.empty.dia 2> %t.empty.txt

// RUN: %FileCheck -check-prefix=GENERAL-ERROR-OCCURRED %s <%t.main.txt
// RUN: %FileCheck -check-prefix=GENERAL-ERROR-OCCURRED %s <%t.empty.txt
// GENERAL-ERROR-OCCURRED: compilation stopped by errors in other files


func test(x: SomeType) {
}
