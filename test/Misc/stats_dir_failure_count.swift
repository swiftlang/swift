// Check that a failed process-tree emits nonzero failure counters
// RUN: %empty-directory(%t)
// RUN: echo zzz >%t/other.swift
// RUN: not %target-swiftc_driver -D BROKEN -j 2 -typecheck -stats-output-dir %t %s %t/other.swift
// RUN: %utils/process-stats-dir.py --set-csv-baseline %t/stats.csv %t
// RUN: %FileCheck -input-file %t/stats.csv -check-prefix=FAILURE %s
// FAILURE: {{"Driver.NumProcessFailures"	1$}}
// FAILURE: {{"Frontend.NumProcessFailures"	2$}}

// Check that a successful process-tree emits no nonzero failure counters
// RUN: %empty-directory(%t)
// RUN: echo 'let x : Int = 1' >%t/other.swift
// RUN: %target-swiftc_driver -j 2 -typecheck -stats-output-dir %t %s %t/other.swift
// RUN: %utils/process-stats-dir.py --set-csv-baseline %t/stats.csv %t
// RUN: %FileCheck -input-file %t/stats.csv -check-prefix=SUCCESS %s
// SUCCESS-NOT: {{"Driver.NumProcessFailures"	[1-9]+}}
// SUCCESS-NOT: {{"Frontend.NumProcessFailures"	[1-9]+}}

func foo() {
#if BROKEN
  print(bar)
#else
  print(1)
#endif
}
