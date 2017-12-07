// REQUIRES: rdar35537905
// Check that a failed process-tree emits nonzero failure counters
// RUN: rm -rf %t && mkdir -p %t
// RUN: echo zzz >%t/other.swift
// RUN: not %target-swiftc_driver -D BROKEN -j 2 -typecheck -stats-output-dir %t %s %t/other.swift
// RUN: %utils/process-stats-dir.py --set-csv-baseline %t/stats.csv %t
// RUN: %FileCheck -input-file %t/stats.csv -check-prefix=FAILURE %s
// FAILURE: {{"Driver.NumProcessFailures"	1$}}
// FAILURE: {{"Frontend.NumProcessFailures"	2$}}

// Check that a successful process-tree emits no failure counters
// RUN: rm -rf %t && mkdir -p %t
// RUN: echo 'let x : Int = 1' >%t/other.swift
// RUN: %target-swiftc_driver -j 2 -typecheck -stats-output-dir %t %s %t/other.swift
// RUN: %utils/process-stats-dir.py --set-csv-baseline %t/stats.csv %t
// RUN: %FileCheck -input-file %t/stats.csv -check-prefix=SUCCESS %s
// SUCCESS-NOT: {{"Driver.NumProcessFailures"}}
// SUCCESS-NOT: {{"Frontend.NumProcessFailures"}}

func foo() {
#if BROKEN
  print(bar)
#else
  print(1)
#endif
}
