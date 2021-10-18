// RUN: %target-build-swift -typecheck -driver-time-compilation %s 2>&1 | %FileCheck %s
// RUN: %target-build-swift -typecheck -driver-time-compilation %s %S/../Inputs/empty.swift 2>&1 | %FileCheck -check-prefix CHECK-MULTIPLE %s

// REQUIRES: rdar82895550

// CHECK: Driver Compilation Time
// CHECK: Total Execution Time: {{[0-9]+}}.{{[0-9]+}} seconds ({{[0-9]+}}.{{[0-9]+}} wall clock)
// CHECK: ---User Time---
// CHECK-SAME: --System Time--
// CHECK-SAME: --User+System--
// CHECK-SAME: ---Wall Time---
// CHECK-SAME: ---Instr---
// CHECK-SAME: --- Name ---
// CHECK-MULTIPLE: {compile: {{.*}}empty.swift}
// CHECK: {{[0-9]+}}.{{[0-9]+}} (100.0%) {{[0-9]+}}.{{[0-9]+}} (100.0%) {{[0-9]+}}.{{[0-9]+}} (100.0%) {{[0-9]+}}.{{[0-9]+}} (100.0%) {{.*}} {compile: {{.*}}driver-time-compilation.swift}
