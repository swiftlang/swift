// Ensure that an error in a non-primary causes an error in the errorless primary.
//
// RUN: rm -f %t.*

// RUN: not %target-swift-frontend -typecheck  -primary-file %s  -serialize-diagnostics-path %t.main.dia -primary-file %S/../Inputs/empty.swift  -serialize-diagnostics-path %t.empty.dia %S/Inputs/serialized-diagnostics-batch-mode-suppression-helper.swift  2> %t.stderr.txt
// RUN: c-index-test -read-diagnostics %t.main.dia 2> %t.main.txt

// RUN: %FileCheck -check-prefix=NO-GENERAL-ERROR-OCCURRED %s <%t.main.txt
// RUN: test -e %t.empty.dia -a ! -s %t.empty.dia
// GENERAL-ERROR-OCCURRED: compilation stopped by errors in other files
// NO-GENERAL-ERROR-OCCURRED-NOT: compilation stopped by errors in other files


func test(x: SomeType) {
    nonexistant()
}
