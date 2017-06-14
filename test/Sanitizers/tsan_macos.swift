// RUN: %target-build-swift %S/Inputs/tsan.swift -target x86_64-apple-macosx10.9 -g -sanitize=thread -o %t_tsan-binary
// RUN: not env TSAN_OPTIONS=abort_on_error=0 %target-run %t_tsan-binary 2>&1 | %FileCheck %s

// XFAIL: linux
// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: tsan_runtime

// UNSUPPORTED: OS=ios
// UNSUPPORTED: OS=tvos
// UNSUPPORTED: OS=watchos

// CHECK: ThreadSanitizer: data race
