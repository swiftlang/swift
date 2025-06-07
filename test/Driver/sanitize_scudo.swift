// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=scudo -target x86_64-unknown-linux-gnu %s | %FileCheck -check-prefix=SCUDO_LINUX %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=scudo -target arm64-apple-ios7.1 %s 2>&1 | %FileCheck -check-prefix=SCUDO_IOS %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=scudo -target arm64-apple-tvos9.0 %s 2>&1 | %FileCheck -check-prefix=SCUDO_tvOS %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=scudo -target armv7k-apple-watchos2.0 %s 2>&1 | %FileCheck -check-prefix=SCUDO_watchOS %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=scudo -target i386-apple-watchos2.0-simulator %s 2>&1 | %FileCheck -check-prefix=SCUDO_watchOS_SIM %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=scudo -target i386-apple-macosx10.9 %s 2>&1 | %FileCheck -check-prefix=SCUDO_OSX_32 %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=scudo -target x86_64-apple-ios7.1-simulator %s 2>&1 | %FileCheck -check-prefix=SCUDO_IOSSIM %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=scudo -target x86_64-apple-macosx10.9 %s 2>&1 | %FileCheck -check-prefix=SCUDO_OSX_64 %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=scudo -target x86_64-apple-tvos9.0-simulator %s 2>&1 | %FileCheck -check-prefix=SCUDO_tvOS_SIM %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=scudo -target x86_64-unknown-windows-msvc %s 2>&1 | %FileCheck -check-prefix=SCUDO_WINDOWS %s

// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=scudo,address -target x86_64-unknown-linux-gnu %s 2>&1 | %FileCheck -check-prefix=SCUDO_ASAN %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=scudo,thread -target x86_64-unknown-linux-gnu %s 2>&1 | %FileCheck -check-prefix=SCUDO_TSAN %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=scudo,undefined -target x86_64-unknown-linux-gnu %s 2>&1 | %FileCheck -check-prefix=SCUDO_UBSAN %s


/*
 * Make sure we don't accidentally add the sanitizer library path when building libraries or modules
 */
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -emit-library -sanitize=scudo -target x86_64-unknown-linux-gnu %s 2>&1 | %FileCheck -check-prefix=SCUDO_LIBRARY_LINUX %s

// SCUDO_LINUX: bin{{/|\\\\}}clang
// SCUDO_LINUX-SAME: -pie
// SCUDO_LINUX-SAME: -fsanitize=scudo
// SCUDO_OSX_32: unsupported option '-sanitize=scudo' for target 'i386-apple-macosx10.9'
// SCUDO_OSX_64: unsupported option '-sanitize=scudo' for target 'x86_64-apple-macosx10.9'
// SCUDO_IOSSIM: unsupported option '-sanitize=scudo' for target 'x86_64-apple-ios7.1-simulator'
// SCUDO_IOS: unsupported option '-sanitize=scudo' for target 'arm64-apple-ios7.1'
// SCUDO_tvOS_SIM: unsupported option '-sanitize=scudo' for target 'x86_64-apple-tvos9.0-simulator'
// SCUDO_tvOS: unsupported option '-sanitize=scudo' for target 'arm64-apple-tvos9.0'
// SCUDO_watchOS_SIM: unsupported option '-sanitize=scudo' for target 'i386-apple-watchos2.0-simulator'
// SCUDO_watchOS: unsupported option '-sanitize=scudo' for target 'armv7k-apple-watchos2.0'
// SCUDO_WINDOWS: unsupported option '-sanitize=scudo' for target 'x86_64-unknown-windows-msvc'

// SCUDO_ASAN: argument '-sanitize=scudo' is not allowed with '-sanitize=address'
// SCUDO_TSAN: argument '-sanitize=scudo' is not allowed with '-sanitize=thread'
// SCUDO_UBSAN: argument '-sanitize=scudo' is not allowed with '-sanitize=undefined'
// SCUDO_LIBRARY_LINUX: bin{{/|\\\\}}clang
// SCUDO_LIBRARY_LINUX-NOT: -fsanitize=scudo
