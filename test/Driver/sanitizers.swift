// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -target x86_64-unknown-linux-gnu %s 2>&1 | %FileCheck -check-prefix=ASAN_LINUX %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=thread -target x86_64-unknown-linux-gnu %s 2>&1 | %FileCheck -check-prefix=TSAN_LINUX %s

// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address,unknown %s 2>&1 | %FileCheck -check-prefix=BADARG %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -sanitize=unknown %s 2>&1 | %FileCheck -check-prefix=BADARG %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address,thread %s 2>&1 | %FileCheck -check-prefix=INCOMPATIBLESANITIZERS %s

// BADARG: unsupported argument 'unknown' to option '-sanitize='
// INCOMPATIBLESANITIZERS: argument '-sanitize=address' is not allowed with '-sanitize=thread'

// ASAN_LINUX: unsupported option '-sanitize=address' for target 'x86_64-unknown-linux-gnu'
// TSAN_LINUX: unsupported option '-sanitize=thread' for target 'x86_64-unknown-linux-gnu'
