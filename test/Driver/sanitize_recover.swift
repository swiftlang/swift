// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -sanitize-recover=foo %s 2>&1 | %FileCheck -check-prefix=SAN_RECOVER_INVALID_ARG %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -sanitize-recover=thread %s 2>&1 | %FileCheck -check-prefix=SAN_RECOVER_UNSUPPORTED_ARG %s
// RUN: %swiftc_driver -v -sanitize-recover=address %s -o %t 2>&1 | %FileCheck -check-prefix=SAN_RECOVER_MISSING_INSTRUMENTATION_OPTION %s
// RUN: %swiftc_driver -driver-print-jobs -sanitize=address -sanitize-recover=address %s 2>&1 | %FileCheck -check-prefix=ASAN_WITH_RECOVER  %s
// RUN: %swiftc_driver -driver-print-jobs -sanitize=address %s 2>&1 | %FileCheck -check-prefix=ASAN_WITHOUT_RECOVER --implicit-check-not='-sanitize-recover=address' %s
// REQUIRES: asan_runtime

// SAN_RECOVER_INVALID_ARG: unsupported argument 'foo' to option '-sanitize-recover='
// SAN_RECOVER_UNSUPPORTED_ARG: unsupported argument 'thread' to option '-sanitize-recover='

// SAN_RECOVER_MISSING_INSTRUMENTATION_OPTION: warning: option '-sanitize-recover=address' has no effect when 'address' sanitizer is disabled. Use -sanitize=address to enable the sanitizer
// SAN_RECOVER_MISSING_INSTRUMENTATION_OPTION-NOT: warning: option '-sanitize-recover=address' has no effect when 'address' sanitizer is disabled. Use -sanitize=address to enable the sanitizer

// ASAN_WITH_RECOVER: swift
// ASAN_WITH_RECOVER-DAG: -sanitize=address
// ASAN_WITH_RECOVER-DAG: -sanitize-recover=address

// ASAN_WITHOUT_RECOVER: swift
// ASAN_WITHOUT_RECOVER: -sanitize=address
