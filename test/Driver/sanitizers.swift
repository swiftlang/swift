// RUN: %swiftc_driver -driver-print-jobs -sanitize=address -target x86_64-apple-macosx10.9 %s | FileCheck -check-prefix=ASAN -check-prefix=OSX %s

// RUN: %swiftc_driver -driver-print-jobs -sanitize=address -target x86_64-apple-ios7.1 %s | FileCheck -check-prefix=ASAN -check-prefix=IOSSIM %s
// RUN: %swiftc_driver -driver-print-jobs -sanitize=address -target arm64-apple-ios7.1 %s  | FileCheck -check-prefix=ASAN -check-prefix=IOS %s

// RUN: %swiftc_driver -driver-print-jobs -sanitize=address -target x86_64-apple-tvos9.0 %s | FileCheck -check-prefix=ASAN -check-prefix=tvOS_SIM %s
// RUN: %swiftc_driver -driver-print-jobs -sanitize=address -target arm64-apple-tvos9.0 %s  | FileCheck -check-prefix=ASAN -check-prefix=tvOS %s

// RUN: %swiftc_driver -driver-print-jobs -sanitize=address -target i386-apple-watchos2.0 %s   | FileCheck -check-prefix=ASAN -check-prefix=watchOS_SIM %s
// RUN: %swiftc_driver -driver-print-jobs -sanitize=address -target armv7k-apple-watchos2.0 %s | FileCheck -check-prefix=ASAN -check-prefix=watchOS %s

// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -target x86_64-unknown-linux-gnu %s 2>&1 | FileCheck -check-prefix=LINUX %s

// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address,unknown %s 2>&1 | FileCheck -check-prefix=BADARG %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=address -sanitize=unknown %s 2>&1 | FileCheck -check-prefix=BADARG %s

// ASAN: swift
// ASAN: -sanitize=address

// OSX: lib/swift/clang/lib/darwin/libclang_rt.asan_osx_dynamic.dylib
// IOS: lib/swift/clang/lib/darwin/libclang_rt.asan_ios_dynamic.dylib
// IOSSIM: lib/swift/clang/lib/darwin/libclang_rt.asan_iossim_dynamic.dylib
// tvOS: lib/swift/clang/lib/darwin/libclang_rt.asan_tvos_dynamic.dylib
// tvOS_SIM: lib/swift/clang/lib/darwin/libclang_rt.asan_tvossim_dynamic.dylib
// watchOS: lib/swift/clang/lib/darwin/libclang_rt.asan_watchos_dynamic.dylib
// watchOS_SIM: lib/swift/clang/lib/darwin/libclang_rt.asan_watchossim_dynamic.dylib
// LINUX: unsupported option 'sanitize=address' for target 'x86_64-unknown-linux-gnu'

// ASAN: -rpath @executable_path

// BADARG: unsupported argument 'unknown' to option 'sanitize='