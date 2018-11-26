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
//
// The following test is verifying that -disable-batch-mode overrides an earlier
// -enable-batch-mode and silences the warning about mixing batch mode with
// -index-file. Tools that take an existing command line and add -index-file can
// thus add -disable-batch-mode without having to otherwise interpret the
// arguments.
//
// RUN: %swiftc_driver -disable-batch-mode -index-file  %S/../Inputs/empty.swift -### 2>%t/stderr_nobatch_index | %FileCheck %s
// RUN: %swiftc_driver -enable-batch-mode -index-file  %S/../Inputs/empty.swift -disable-batch-mode -### 2>%t/stderr_batch_nobatch_index | %FileCheck %s
// RUN: %FileCheck -allow-empty -check-prefix CHECK-INDEX-DISABLED %s <%t/stderr_nobatch_index
// RUN: %FileCheck -allow-empty -check-prefix CHECK-INDEX-DISABLED %s <%t/stderr_batch_nobatch_index
// CHECK-INDEX-DISABLED-NOT: warning
//
// This next one is a regression test for a specific failure in the past: wmo +
// batch mode should not just result in wmo, but also preserve the num-threads
// argument and (crucially) the resulting fact that the single wmo subprocess
// generates multiple output files. The build system that invokes swiftc expects
// multiple outputs.
//
// RUN: touch %t/a.swift %t/b.swift %t/c.swift
// RUN: %swiftc_driver %t/a.swift %t/b.swift %t/c.swift -num-threads 4 -whole-module-optimization -enable-batch-mode -### >%t/stdout_mt_wmo 2>%t/stderr_mt_wmo
// RUN: %FileCheck --check-prefix CHECK-WMO %s <%t/stderr_mt_wmo
// RUN: %FileCheck --check-prefix CHECK-MULTITHREADED-WMO-ARGS %s <%t/stdout_mt_wmo
// CHECK-MULTITHREADED-WMO-ARGS: -num-threads 4 {{.*}}-o {{.*}}/a-{{[a-z0-9]+}}.o -o {{.*}}/b-{{[a-z0-9]+}}.o -o {{.*}}/c-{{[a-z0-9]+}}.o
