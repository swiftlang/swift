// RUN: %empty-directory(%t)
// RUN: ( %target-build-swift %s -o %t/NotImportedByDefault || true ) 2>&1 | %FileCheck %s

// Windows chokes on the parens in the above expression
// UNSUPPORTED: OS=windows-msvc

// REQUIRES: backtracing

let backtrace = try! Backtrace.capture()

// CHECK: error: cannot find 'Backtrace' in scope
