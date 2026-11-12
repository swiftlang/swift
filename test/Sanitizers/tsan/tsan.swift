// RUN: %empty-directory(%t)
// RUN: %target-clang -x c %S/Inputs/tsan_invisible_barrier.c -c -o %t/tsan_invisible_barrier.o
// RUN: %target-swiftc_driver %s %t/tsan_invisible_barrier.o -import-objc-header %S/Inputs/tsan_invisible_barrier.h -g -sanitize=thread %import-libdispatch -o %t_tsan-binary
// RUN: %target-codesign %t_tsan-binary
// RUN: env %env-TSAN_OPTIONS="abort_on_error=0" not %target-run %t_tsan-binary 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: tsan_runtime
// UNSUPPORTED: OS=tvos
// UNSUPPORTED: CPU=powerpc64le
// UNSUPPORTED: threading_none

// rdar://101876380
// UNSUPPORTED: OS=ios

// FIXME: This should be covered by "tsan_runtime"; older versions of Apple OSs
// don't support TSan.
// UNSUPPORTED: remote_run

// TSan is detecting race conditions in the concurrency runtime.
// This might be an issue in how it is hooked into the underlying threading
// model on FreeBSD.
// rdar://158355890
// XFAIL: OS=freebsd

#if canImport(Darwin)
  import Darwin
#elseif canImport(Glibc)
  import Glibc
#elseif os(Windows)
  import CRT
#else
#error("Unsupported platform")
#endif

// Make sure we can handle swifterror and don't bail during the LLVM
// threadsanitizer pass.

enum MyError : Error {
    case A
}

public func foobar(_ x: Int) throws {
  if x == 0 {
    throw MyError.A
  }
}

public func call_foobar() {
  do {
    try foobar(1)
  } catch(_) { }
}

// Test ThreadSanitizer execution end-to-end.

var racey_x: Int;

swift_tsan_invisible_barrier_init(2)

#if os(macOS) || os(iOS) || os(FreeBSD)
var t1: pthread_t?
var t2: pthread_t?
#else
var t1: pthread_t = 0
var t2: pthread_t = 0
#endif

pthread_create(&t1, nil, { _ in
  print("pthread ran")
  racey_x = 5
  swift_tsan_invisible_barrier_wait()
  return nil
}, nil)

pthread_create(&t2, nil, { _ in
  swift_tsan_invisible_barrier_wait()
  print("pthread ran")
  racey_x = 6
  return nil
}, nil)

#if os(macOS) || os(iOS)
pthread_join(t1!, nil)
pthread_join(t2!, nil)
#else
pthread_join(t1, nil)
pthread_join(t2, nil)
#endif

// CHECK: ThreadSanitizer: data race
