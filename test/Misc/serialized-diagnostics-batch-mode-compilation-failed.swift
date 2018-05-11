// To avoid redundant diagnostics showing up in Xcode, batch-mode must suppress diagnostics in
// non-primary files.
// But if the only fatal errors are suppressed ones, a "compilation failed" error must be emitted
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

// RUN: %FileCheck -check-prefix=COMPILATION-FAILED %s <%t.main.txt
// RUN: %FileCheck -check-prefix=COMPILATION-FAILED %s <%t.empty.txt
// COMPILATION-FAILED: compilation failed

func test(x: SomeType) {
}
