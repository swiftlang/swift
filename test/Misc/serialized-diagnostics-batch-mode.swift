// RUN: rm -f %t.*

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
  nonexistent() // CHECK-MAIN-DAG: serialized-diagnostics-batch-mode.swift:[[@LINE]]:3: error: use of unresolved identifier 'nonexistent'
  // CHECK-STDERR-DAG: serialized-diagnostics-batch-mode.swift:[[@LINE-1]]:3: error: use of unresolved identifier 'nonexistent'

  // The other file has a similar call.
  // CHECK-HELPER-DAG: serialized-diagnostics-batch-mode-helper.swift:{{[0-9]+}}:3: error: use of unresolved identifier 'nonexistent'
  // CHECK-STDERR-DAG: serialized-diagnostics-batch-mode-helper.swift:{{[0-9]+}}:3: error: use of unresolved identifier 'nonexistent'
}
