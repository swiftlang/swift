/*
 * Address Sanitizer Tests  (asan)
 */
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=address -target x86_64-apple-macosx10.9 %s | %FileCheck -check-prefix=ASAN -check-prefix=ASAN_OSX %s
// RUN: not %swiftc_driver -driver-print-jobs -sanitize=fuzzer -target x86_64-apple-macosx10.9 -resource-dir %S/Inputs/nonexistent-resource-dir %s 2>&1 | %FileCheck -check-prefix=FUZZER_NONEXISTENT %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=address -target x86_64-apple-ios7.1 %s | %FileCheck -check-prefix=ASAN -check-prefix=ASAN_IOSSIM %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=address -target arm64-apple-ios7.1 %s  | %FileCheck -check-prefix=ASAN -check-prefix=ASAN_IOS %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=address -target x86_64-apple-tvos9.0 %s | %FileCheck -check-prefix=ASAN -check-prefix=ASAN_tvOS_SIM %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=address -target arm64-apple-tvos9.0 %s  | %FileCheck -check-prefix=ASAN -check-prefix=ASAN_tvOS %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=address -target i386-apple-watchos2.0 %s   | %FileCheck -check-prefix=ASAN -check-prefix=ASAN_watchOS_SIM %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=address -target armv7k-apple-watchos2.0 %s | %FileCheck -check-prefix=ASAN -check-prefix=ASAN_watchOS %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=address -target x86_64-unknown-linux-gnu %s 2>&1 | %FileCheck -check-prefix=ASAN -check-prefix=ASAN_LINUX %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=address -target x86_64-unknown-windows-msvc %s 2>&1 | %FileCheck -check-prefix=ASAN -check-prefix=ASAN_WINDOWS %s

/*
 * Thread Sanitizer Tests  (tsan)
 */
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=thread -target x86_64-apple-macosx10.9 %s | %FileCheck -check-prefix=TSAN -check-prefix=TSAN_OSX %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=thread -target x86-apple-macosx10.9 %s 2>&1 | %FileCheck -check-prefix=TSAN_OSX_32 %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=thread -target x86_64-apple-ios7.1 %s 2>&1 | %FileCheck -check-prefix=TSAN_IOSSIM %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=thread -target arm64-apple-ios7.1 %s 2>&1 | %FileCheck -check-prefix=TSAN_IOS %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=thread -target x86_64-apple-tvos9.0 %s 2>&1 | %FileCheck -check-prefix=TSAN_tvOS_SIM %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=thread -target arm64-apple-tvos9.0 %s 2>&1 | %FileCheck -check-prefix=TSAN_tvOS %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=thread -target i386-apple-watchos2.0 %s 2>&1 | %FileCheck -check-prefix=TSAN_watchOS_SIM %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=thread -target armv7k-apple-watchos2.0 %s 2>&1  | %FileCheck -check-prefix=TSAN_watchOS %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=thread -target x86_64-unknown-windows-msvc %s 2>&1 | %FileCheck -check-prefix=TSAN_WINDOWS %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=thread -target x86_64-unknown-linux-gnu %s 2>&1 | %FileCheck -check-prefix=TSAN_LINUX %s

/*
 * Undefined Behavior Sanitizer Tests  (ubsan)
 */
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=undefined -target x86_64-apple-macosx10.9 %s | %FileCheck -check-prefix=UBSAN -check-prefix=UBSAN_OSX %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=undefined -target x86_64-apple-ios7.1 %s | %FileCheck -check-prefix=UBSAN -check-prefix=UBSAN_IOSSIM %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=undefined -target arm64-apple-ios7.1 %s  | %FileCheck -check-prefix=UBSAN -check-prefix=UBSAN_IOS %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=undefined -target x86_64-apple-tvos9.0 %s | %FileCheck -check-prefix=UBSAN -check-prefix=UBSAN_tvOS_SIM %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=undefined -target arm64-apple-tvos9.0 %s  | %FileCheck -check-prefix=UBSAN -check-prefix=UBSAN_tvOS %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=undefined -target i386-apple-watchos2.0 %s   | %FileCheck -check-prefix=UBSAN -check-prefix=UBSAN_watchOS_SIM %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=undefined -target armv7k-apple-watchos2.0 %s | %FileCheck -check-prefix=UBSAN -check-prefix=UBSAN_watchOS %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=undefined -target x86_64-unknown-linux-gnu %s 2>&1 | %FileCheck -check-prefix=UBSAN -check-prefix=UBSAN_LINUX %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=undefined -target x86_64-unknown-windows-msvc %s 2>&1 | %FileCheck -check-prefix=UBSAN -check-prefix=UBSAN_WINDOWS %s


/*
 * Bad Argument Tests
 */
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=address,unknown %s 2>&1 | %FileCheck -check-prefix=BADARG %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=address -sanitize=unknown %s 2>&1 | %FileCheck -check-prefix=BADARG %s
// RUN: not %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -sanitize=address,thread %s 2>&1 | %FileCheck -check-prefix=INCOMPATIBLESANITIZERS %s

/*
 * Make sure we don't accidentally add the sanitizer library path when building libraries or modules
 */
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -emit-library -sanitize=address -target x86_64-unknown-linux-gnu %s 2>&1 | %FileCheck --implicit-check-not=ASAN_LINUX %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -emit-module -sanitize=address -target x86_64-unknown-linux-gnu %s 2>&1 | %FileCheck --implicit-check-not=ASAN_LINUX %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -emit-library -sanitize=thread -target x86_64-unknown-linux-gnu %s 2>&1 | %FileCheck --implicit-check-not=TSAN_LINUX %s
// RUN: %swiftc_driver -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -driver-print-jobs -emit-module -sanitize=thread -target x86_64-unknown-linux-gnu %s 2>&1 | %FileCheck --implicit-check-not=TSAN_LINUX %s

// ASAN: swift
// ASAN: -sanitize=address

// ASAN_OSX: lib/swift/clang/lib/darwin/libclang_rt.asan_osx_dynamic.dylib
// ASAN_IOSSIM: lib/swift/clang/lib/darwin/libclang_rt.asan_iossim_dynamic.dylib
// ASAN_IOS: lib/swift/clang/lib/darwin/libclang_rt.asan_ios_dynamic.dylib
// ASAN_tvOS_SIM: lib/swift/clang/lib/darwin/libclang_rt.asan_tvossim_dynamic.dylib
// ASAN_tvOS: lib/swift/clang/lib/darwin/libclang_rt.asan_tvos_dynamic.dylib
// ASAN_watchOS_SIM: lib/swift/clang/lib/darwin/libclang_rt.asan_watchossim_dynamic.dylib
// ASAN_watchOS: lib/swift/clang/lib/darwin/libclang_rt.asan_watchos_dynamic.dylib
// ASAN_LINUX: lib/swift/clang/lib/linux/libclang_rt.asan-x86_64.a
// ASAN_WINDOWS: lib/swift/clang/lib/windows/clang_rt.asan-x86_64.lib

// ASAN: -rpath @executable_path

// TSAN: swift
// TSAN: -sanitize=thread

// TSAN_OSX: lib/swift/clang/lib/darwin/libclang_rt.tsan_osx_dynamic.dylib
// TSAN_OSX_32: unsupported option '-sanitize=thread' for target 'x86-apple-macosx10.9'
// TSAN_IOSSIM: unsupported option '-sanitize=thread' for target 'x86_64-apple-ios7.1'
// TSAN_IOS: unsupported option '-sanitize=thread' for target 'arm64-apple-ios7.1'
// TSAN_tvOS_SIM: unsupported option '-sanitize=thread' for target 'x86_64-apple-tvos9.0'
// TSAN_tvOS: unsupported option '-sanitize=thread' for target 'arm64-apple-tvos9.0'
// TSAN_watchOS_SIM: unsupported option '-sanitize=thread' for target 'i386-apple-watchos2.0'
// TSAN_watchOS: unsupported option '-sanitize=thread' for target 'armv7k-apple-watchos2.0'
// FUZZER_NONEXISTENT: unsupported option '-sanitize=fuzzer' for target 'x86_64-apple-macosx10.9'
// TSAN_LINUX: lib/swift/clang/lib/linux/libclang_rt.tsan-x86_64.a
// TSAN_WINDOWS: unsupported option '-sanitize=thread' for target 'x86_64-unknown-windows-msvc'

// TSAN: -rpath @executable_path

// UBSAN: swift
// UBSAN: -sanitize=undefined

// UBSAN_OSX: lib/swift/clang/lib/darwin/libclang_rt.ubsan_osx_dynamic.dylib
// UBSAN_IOSSIM: lib/swift/clang/lib/darwin/libclang_rt.ubsan_iossim_dynamic.dylib
// UBSAN_IOS: lib/swift/clang/lib/darwin/libclang_rt.ubsan_ios_dynamic.dylib
// UBSAN_tvOS_SIM: lib/swift/clang/lib/darwin/libclang_rt.ubsan_tvossim_dynamic.dylib
// UBSAN_tvOS: lib/swift/clang/lib/darwin/libclang_rt.ubsan_tvos_dynamic.dylib
// UBSAN_watchOS_SIM: lib/swift/clang/lib/darwin/libclang_rt.ubsan_watchossim_dynamic.dylib
// UBSAN_watchOS: lib/swift/clang/lib/darwin/libclang_rt.ubsan_watchos_dynamic.dylib
// UBSAN_LINUX: lib/swift/clang/lib/linux/libclang_rt.ubsan-x86_64.a
// UBSAN_WINDOWS: lib/swift/clang/lib/windows/clang_rt.ubsan_standalone-x86_64.lib

// UBSAN: -rpath @executable_path

// BADARG: unsupported argument 'unknown' to option '-sanitize='
// INCOMPATIBLESANITIZERS: argument '-sanitize=address' is not allowed with '-sanitize=thread'
