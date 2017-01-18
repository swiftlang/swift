// RUN: %swiftc_driver -driver-print-jobs -sanitize=address -target x86_64-apple-macosx10.9 %s | %FileCheck -check-prefix=ASAN -check-prefix=ASAN_OSX %s
// RUN: %swiftc_driver -driver-print-jobs -sanitize=address -target x86_64-apple-ios7.1 %s | %FileCheck -check-prefix=ASAN -check-prefix=ASAN_IOSSIM %s
// RUN: %swiftc_driver -driver-print-jobs -sanitize=address -target arm64-apple-ios7.1 %s  | %FileCheck -check-prefix=ASAN -check-prefix=ASAN_IOS %s
// RUN: %swiftc_driver -driver-print-jobs -sanitize=address -target x86_64-apple-tvos9.0 %s | %FileCheck -check-prefix=ASAN -check-prefix=ASAN_tvOS_SIM %s
// RUN: %swiftc_driver -driver-print-jobs -sanitize=address -target arm64-apple-tvos9.0 %s  | %FileCheck -check-prefix=ASAN -check-prefix=ASAN_tvOS %s
// RUN: %swiftc_driver -driver-print-jobs -sanitize=address -target i386-apple-watchos2.0 %s   | %FileCheck -check-prefix=ASAN -check-prefix=ASAN_watchOS_SIM %s
// RUN: %swiftc_driver -driver-print-jobs -sanitize=address -target armv7k-apple-watchos2.0 %s | %FileCheck -check-prefix=ASAN -check-prefix=ASAN_watchOS %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -target x86_64-unknown-linux-gnu %s 2>&1 | %FileCheck -check-prefix=ASAN_LINUX %s

// RUN: %swiftc_driver -driver-print-jobs -sanitize=thread -target x86_64-apple-macosx10.9 %s | %FileCheck -check-prefix=TSAN -check-prefix=TSAN_OSX %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=thread -target x86-apple-macosx10.9 %s 2>&1 | %FileCheck -check-prefix=TSAN_OSX_32 %s
// RUN: %swiftc_driver -driver-print-jobs -sanitize=thread -target x86_64-apple-ios7.1 %s | %FileCheck -check-prefix=TSAN -check-prefix=TSAN_IOSSIM %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=thread -target arm64-apple-ios7.1 %s 2>&1 | %FileCheck -check-prefix=TSAN_IOS %s
// RUN: %swiftc_driver -driver-print-jobs -sanitize=thread -target x86_64-apple-tvos9.0 %s | %FileCheck -check-prefix=TSAN -check-prefix=TSAN_tvOS_SIM %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=thread -target arm64-apple-tvos9.0 %s 2>&1 | %FileCheck -check-prefix=TSAN_tvOS %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=thread -target i386-apple-watchos2.0 %s 2>&1 | %FileCheck -check-prefix=TSAN_watchOS_SIM %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=thread -target armv7k-apple-watchos2.0 %s 2>&1  | %FileCheck -check-prefix=TSAN_watchOS %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=thread -target x86_64-unknown-linux-gnu %s 2>&1 | %FileCheck -check-prefix=TSAN_LINUX %s

// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address,unknown %s 2>&1 | %FileCheck -check-prefix=BADARG %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -sanitize=unknown %s 2>&1 | %FileCheck -check-prefix=BADARG %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address,thread %s 2>&1 | %FileCheck -check-prefix=INCOMPATIBLESANITIZERS %s

// ASAN: swift
// ASAN: -sanitize=address

// ASAN_OSX: lib/swift/clang/lib/darwin/libclang_rt.asan_osx_dynamic.dylib
// ASAN_IOSSIM: lib/swift/clang/lib/darwin/libclang_rt.asan_iossim_dynamic.dylib
// ASAN_IOS: lib/swift/clang/lib/darwin/libclang_rt.asan_ios_dynamic.dylib
// ASAN_tvOS_SIM: lib/swift/clang/lib/darwin/libclang_rt.asan_tvossim_dynamic.dylib
// ASAN_tvOS: lib/swift/clang/lib/darwin/libclang_rt.asan_tvos_dynamic.dylib
// ASAN_watchOS_SIM: lib/swift/clang/lib/darwin/libclang_rt.asan_watchossim_dynamic.dylib
// ASAN_watchOS: lib/swift/clang/lib/darwin/libclang_rt.asan_watchos_dynamic.dylib
// ASAN_LINUX: unsupported option '-sanitize=address' for target 'x86_64-unknown-linux-gnu'

// ASAN: -rpath @executable_path

// TSAN: swift
// TSAN: -sanitize=thread

// TSAN_OSX: lib/swift/clang/lib/darwin/libclang_rt.tsan_osx_dynamic.dylib
// TSAN_OSX_32: unsupported option '-sanitize=thread' for target 'x86-apple-macosx10.9'
// TSAN_IOSSIM: lib/swift/clang/lib/darwin/libclang_rt.tsan_iossim_dynamic.dylib
// TSAN_IOS: unsupported option '-sanitize=thread' for target 'arm64-apple-ios7.1'
// TSAN_tvOS_SIM: lib/swift/clang/lib/darwin/libclang_rt.tsan_tvossim_dynamic.dylib
// TSAN_tvOS: unsupported option '-sanitize=thread' for target 'arm64-apple-tvos9.0'
// TSAN_watchOS_SIM: unsupported option '-sanitize=thread' for target 'i386-apple-watchos2.0'
// TSAN_watchOS: unsupported option '-sanitize=thread' for target 'armv7k-apple-watchos2.0'
// TSAN_LINUX: unsupported option '-sanitize=thread' for target 'x86_64-unknown-linux-gnu'

// TSAN: -rpath @executable_path

// BADARG: unsupported argument 'unknown' to option '-sanitize='
// INCOMPATIBLESANITIZERS: argument '-sanitize=address' is not allowed with '-sanitize=thread'
