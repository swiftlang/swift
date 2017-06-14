// REQUIRES: OS=macosx

// RUN: %swiftc_driver -driver-print-jobs -sanitize=address -target x86_64-apple-macosx10.9 %s | %FileCheck %s --check-prefix=ASAN

// ASAN: swift
// ASAN-FLAG: -sanitize=address
// ASAN-LIB: lib/swift/clang/lib/darwin/libclang_rt.asan_osx_dynamic.dylib
// ASAN-RPATH: -rpath @executable_path

// RUN: %swiftc_driver -driver-print-jobs -sanitize=thread -target x86_64-apple-macosx10.9 %s | %FileCheck %s --check-prefix=TSAN

// TSAN: swift
// TSAN-FLAG: -sanitize=thread
// TSAN-LIB: lib/swift/clang/lib/darwin/libclang_rt.tsan_osx_dynamic.dylib
// TSAN-RPATH: -rpath @executable_path

// RUN: not %swiftc_driver -driver-print-jobs -sanitize=thread -target x86-apple-macosx10.9 %s 2>&1 | %FileCheck %s --check-prefix=TSAN32

// TSAN32: unsupported option '-sanitize=thread' for target 'x86-apple-macosx10.9'
