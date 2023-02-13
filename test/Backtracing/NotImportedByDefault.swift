// RUN: %empty-directory(%t)
// RUN: ( %target-build-swift %s -o %t/NotImportedByDefault || true) 2>&1 | %FileCheck %s

let backtrace = try! Backtrace.capture()

// CHECK: error: cannot find 'Backtrace' in scope
