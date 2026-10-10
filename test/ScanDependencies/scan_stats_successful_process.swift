// The in-process dependency scan writes statistics like a frontend process.
// A successful scan must not count as a failed process.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/stats)
// RUN: %target-swiftc_driver -explicit-module-build -typecheck -module-cache-path %t/module-cache \
// RUN:   -stats-output-dir %t/stats %s
// RUN: %{python} %utils/process-stats-dir.py --set-csv-baseline %t/stats.csv %t/stats
// RUN: %FileCheck -input-file %t/stats.csv %s

// CHECK: {{"AST\.}}
// CHECK-NOT: {{"Frontend.NumProcessFailures"	[1-9]+}}

let x = 1
