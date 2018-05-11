// To avoid redundant diagnostics showing up in Xcode, batch-mode must suppress diagnostics in
// non-primary files.
//
// RUN: rm -f %t.*

// RUN: not %target-swift-frontend -typecheck  -primary-file %s  -serialize-diagnostics-path %t.main.dia -primary-file %S/../Inputs/empty.swift  -serialize-diagnostics-path %t.empty.dia %S/Inputs/serialized-diagnostics-batch-mode-suppression-helper.swift  2> %t.stderr.txt
// RUN: c-index-test -read-diagnostics %t.main.dia 2> %t.main.txt
// RUN: c-index-test -read-diagnostics %t.empty.dia 2> %t.empty.txt

// RUN: %FileCheck -check-prefix=NO-COMPILATION-FAILED %s <%t.main.txt
// RUN: %FileCheck -check-prefix=NO-COMPILATION-FAILED %s <%t.empty.txt
// NO-COMPILATION-FAILED-NOT: compilation failed


func test(x: SomeType) {
  nonexistant(); // create a fatal error here
}
