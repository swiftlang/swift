// REQUIRES: disabled
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -target x86_64-apple-macosx10.9 %s 2>&1 | FileCheck -check-prefix=ASAN %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -target x86_64-apple-ios7.1 %s 2>&1 | FileCheck -check-prefix=ASAN %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -target arm64-apple-ios7.1 %s 2>&1 | FileCheck -check-prefix=ASAN %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -target x86_64-apple-tvos9.0 %s 2>&1 | FileCheck -check-prefix=ASAN %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -target arm64-apple-tvos9.0 %s 2>&1 | FileCheck -check-prefix=ASAN %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -target i386-apple-watchos2.0 %s 2>&1 | FileCheck -check-prefix=ASAN %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -target armv7k-apple-watchos2.0 2>&1 %s | FileCheck -check-prefix=ASAN %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -target x86_64-unknown-linux-gnu %s 2>&1 | FileCheck -check-prefix=ASAN %s

// RUN: not %swiftc_driver -driver-print-jobs -sanitize=thread -target x86_64-apple-macosx10.9 %s 2>&1 | FileCheck -check-prefix=TSAN %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=thread -target x86-apple-macosx10.9 %s 2>&1 | FileCheck -check-prefix=TSAN %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=thread -target x86_64-apple-ios7.1 %s 2>&1 | FileCheck -check-prefix=TSAN %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=thread -target arm64-apple-ios7.1 %s 2>&1 | FileCheck -check-prefix=TSAN %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=thread -target x86_64-apple-tvos9.0 %s 2>&1 | FileCheck -check-prefix=TSAN %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=thread -target arm64-apple-tvos9.0 %s 2>&1 | FileCheck -check-prefix=TSAN %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=thread -target i386-apple-watchos2.0 %s 2>&1 | FileCheck -check-prefix=TSAN %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=thread -target armv7k-apple-watchos2.0 %s 2>&1  | FileCheck -check-prefix=TSAN %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=thread -target x86_64-unknown-linux-gnu %s 2>&1 | FileCheck -check-prefix=TSAN %s

// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address,unknown %s 2>&1 | FileCheck -check-prefix=BADARG %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -sanitize=unknown %s 2>&1 | FileCheck -check-prefix=BADARG %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address,thread %s 2>&1 | FileCheck -check-prefix=INCOMPATIBLESANITIZERS %s

// ASAN: argument '-sanitize=address' is not supported on the Swift 2.3 toolchain. You will need to migrate your project to Swift 3 to use this feature.
// TSAN: argument '-sanitize=thread' is not supported on the Swift 2.3 toolchain. You will need to migrate your project to Swift 3 to use this feature.

// BADARG: unsupported argument 'unknown' to option '-sanitize='
// INCOMPATIBLESANITIZERS: argument '-sanitize=address' is not supported on the Swift 2.3 toolchain. You will need to migrate your project to Swift 3 to use this feature.
// INCOMPATIBLESANITIZERS: argument '-sanitize=thread' is not supported on the Swift 2.3 toolchain. You will need to migrate your project to Swift 3 to use this feature.
