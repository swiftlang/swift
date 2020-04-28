// RUN: rm -f %t.*

// The `-serialize-diagnostics-path` flag is not allowed for batch mode invoked by swiftc
// RUN: not %target-swiftc_driver -serialize-diagnostics-path %t.notexpected.dia %s %S/Inputs/serialized-diagnostics-batch-mode-helper.swift -c -o %t.o 2>&1 | %FileCheck %s
// CHECK: <unknown>:0: error: option '-serialize-diagnostics-path' is not supported by 'swiftc'; did you mean to use 'swift'?
// RUN: not ls %t.notexpected.dia > /dev/null
// RUN: not ls %t.o > /dev/null


// RUN: not %target-swift-frontend -typecheck -primary-file %s  -serialize-diagnostics-path %t.main.dia -primary-file %S/Inputs/serialized-diagnostics-batch-mode-helper.swift  -serialize-diagnostics-path %t.helper.dia %S/Inputs/serialized-diagnostics-batch-mode-other.swift 2> %t.stderr.txt
// RUN: %FileCheck -check-prefix=CHECK-STDERR %s < %t.stderr.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-STDERR %s < %t.stderr.txt

// RUN: c-index-test -read-diagnostics %t.main.dia 2> %t.main.txt
// RUN: %FileCheck -check-prefix=CHECK-MAIN %s < %t.main.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-MAIN %s < %t.main.txt

// RUN: c-index-test -read-diagnostics %t.helper.dia 2> %t.helper.txt
// RUN: %FileCheck -check-prefix=CHECK-HELPER %s < %t.helper.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-HELPER %s < %t.helper.txt

// NEGATIVE-MAIN-NOT: shouldNotShowUpInOutput
// NEGATIVE-HELPER-NOT: shouldNotShowUpInOutput
// NEGATIVE-STDERR-NOT: shouldNotShowUpInOutput

// NEGATIVE-MAIN-NOT: serialized-diagnostics-batch-mode-helper.swift
// NEGATIVE-HELPER-NOT: serialized-diagnostics-batch-mode.swift

// NEGATIVE-MAIN-NOT: invalid redeclaration of 'foo()'
// NEGATIVE-HELPER-NOT: invalid redeclaration of 'foo()'
// NEGATIVE-STDERR-NOT: invalid redeclaration of 'foo()'

func test() {
  nonexistent() // CHECK-MAIN-DAG: serialized-diagnostics-batch-mode.swift:[[@LINE]]:3: error: cannot find 'nonexistent' in scope
  // CHECK-STDERR-DAG: serialized-diagnostics-batch-mode.swift:[[@LINE-1]]:3: error: cannot find 'nonexistent' in scope

  // The other file has a similar call.
  // CHECK-HELPER-DAG: serialized-diagnostics-batch-mode-helper.swift:{{[0-9]+}}:3: error: cannot find 'nonexistent' in scope
  // CHECK-STDERR-DAG: serialized-diagnostics-batch-mode-helper.swift:{{[0-9]+}}:3: error: cannot find 'nonexistent' in scope
}
