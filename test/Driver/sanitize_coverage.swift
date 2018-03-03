// Different sanitizer coverage types
// RUN: %swiftc_driver -driver-print-jobs -sanitize-coverage=func -sanitize=address %s | %FileCheck -check-prefix=SANCOV_FUNC %s
// RUN: %swiftc_driver -driver-print-jobs -sanitize-coverage=bb -sanitize=address %s | %FileCheck -check-prefix=SANCOV_BB %s
// RUN: %swiftc_driver -driver-print-jobs -sanitize-coverage=edge -sanitize=address %s | %FileCheck -check-prefix=SANCOV_EDGE %s
// REQUIRES: asan_runtime

// Try some options
// RUN: %swiftc_driver -driver-print-jobs -sanitize-coverage=edge,indirect-calls,trace-bb,trace-cmp,8bit-counters  -sanitize=address %s | %FileCheck -check-prefix=SANCOV_EDGE_WITH_OPTIONS %s

// Invalid command line arguments
// RUN: not %swiftc_driver -driver-print-jobs -sanitize-coverage=func %s 2>&1 | %FileCheck -check-prefix=SANCOV_WITHOUT_XSAN %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize-coverage=bb %s 2>&1 | %FileCheck -check-prefix=SANCOV_WITHOUT_XSAN %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize-coverage=edge %s 2>&1 | %FileCheck -check-prefix=SANCOV_WITHOUT_XSAN %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -sanitize-coverage=unknown %s 2>&1 | %FileCheck -check-prefix=SANCOV_BAD_ARG %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -sanitize-coverage=indirect-calls %s 2>&1 | %FileCheck -check-prefix=SANCOV_MISSING_ARG %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -sanitize-coverage=trace-bb %s 2>&1 | %FileCheck -check-prefix=SANCOV_MISSING_ARG %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -sanitize-coverage=trace-cmp %s 2>&1 | %FileCheck -check-prefix=SANCOV_MISSING_ARG %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -sanitize-coverage=8bit-counters %s 2>&1 | %FileCheck -check-prefix=SANCOV_MISSING_ARG %s

// SANCOV_FUNC: swift
// SANCOV_FUNC-DAG: -sanitize-coverage=func
// SANCOV_FUNC-DAG: -sanitize=address

// SANCOV_BB: swift
// SANCOV_BB-DAG: -sanitize-coverage=bb
// SANCOV_BB-DAG: -sanitize=address

// SANCOV_EDGE: swift
// SANCOV_EDGE-DAG: -sanitize-coverage=edge
// SANCOV_EDGE-DAG: -sanitize=address

// SANCOV_EDGE_WITH_OPTIONS: swift
// SANCOV_EDGE_WITH_OPTIONS-DAG: -sanitize-coverage=edge,indirect-calls,trace-bb,trace-cmp,8bit-counters
// SANCOV_EDGE_WITH_OPTIONS-DAG: -sanitize=address

// SANCOV_WITHOUT_XSAN: option '-sanitize-coverage=' requires a sanitizer to be enabled. Use -sanitize= to enable a sanitizer
// SANCOV_BAD_ARG: unsupported argument 'unknown' to option '-sanitize-coverage='
// SANCOV_MISSING_ARG: error: option '-sanitize-coverage=' is missing a required argument ("func", "bb", "edge")
