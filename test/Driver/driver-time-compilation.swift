// RUN: %target-build-swift -typecheck -driver-time-compilation %s 2>&1 | %FileCheck %s
// RUN: %target-build-swift -typecheck -driver-time-compilation %s %S/../Inputs/empty.swift 2>&1 | %FileCheck -check-prefix CHECK-MULTIPLE %s

// CHECK: Driver Compilation Time
// CHECK: Total Execution Time: {{[0-9]+}}.{{[0-9]+}} seconds ({{[0-9]+}}.{{[0-9]+}} wall clock)
// CHECK: ---Wall Time---
// CHECK: --- Name ---
// CHECK: compile {{.*}}driver-time-compilation.swift
// CHECK-MULTIPLE: compile {{.*}}empty.swift
// CHECK: {{[0-9]+}}.{{[0-9]+}} (100.0%)  Total
