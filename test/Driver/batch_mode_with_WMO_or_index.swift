// RUN: %empty-directory(%t)
//
// RUN: %swiftc_driver -whole-module-optimization -enable-batch-mode  %S/../Inputs/empty.swift -### 2>%t/stderr_WMO_batch | %FileCheck %s
// RUN: %swiftc_driver -enable-batch-mode -whole-module-optimization  %S/../Inputs/empty.swift -### 2>%t/stderr_batch_WMO | %FileCheck %s
// CHECK-NOT: -primary-file
// RUN: %FileCheck -check-prefix CHECK-WMO %s <%t/stderr_WMO_batch
// RUN: %FileCheck -check-prefix CHECK-WMO %s <%t/stderr_batch_WMO
// CHECK-WMO: warning: ignoring '-enable-batch-mode' because '-whole-module-optimization' was also specified
//
// RUN: %swiftc_driver -index-file -enable-batch-mode  %S/../Inputs/empty.swift -### 2>%t/stderr_index_batch | %FileCheck %s
// RUN: %swiftc_driver -enable-batch-mode -index-file  %S/../Inputs/empty.swift -### 2>%t/stderr_batch_index | %FileCheck %s
// RUN: %FileCheck -check-prefix CHECK-INDEX %s <%t/stderr_index_batch
// RUN: %FileCheck -check-prefix CHECK-INDEX %s <%t/stderr_batch_index
// CHECK-INDEX: warning: ignoring '-enable-batch-mode' because '-index-file' was also specified
