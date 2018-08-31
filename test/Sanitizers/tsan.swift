// RUN: %target-swiftc_driver %s -target %sanitizers-target-triple -g -sanitize=thread -o %t_tsan-binary
// RUN: %target-codesign %t_tsan-binary
// RUN: not env %env-TSAN_OPTIONS="abort_on_error=0" %target-run %t_tsan-binary 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: tsan_runtime
// UNSUPPORTED: OS=tvos

// https://bugs.swift.org/browse/SR-6622
// XFAIL: linux

#if os(macOS) || os(iOS)
import Darwin
#elseif os(Linux) || os(FreeBSD) || os(PS4) || os(Android) || os(Cygwin)
import Glibc
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

var threads: [pthread_t] = []
var racey_x: Int;

for _ in 1...5 {
#if os(macOS) || os(iOS)
  var t : pthread_t?
#else
  var t : pthread_t = 0
#endif
  pthread_create(&t, nil, { _ in
    print("pthread ran")
    racey_x = 5;

    return nil
  }, nil)
#if os(macOS) || os(iOS)
  threads.append(t!)
#else
  threads.append(t)
#endif
}

for t in threads {
  if t == nil {
    print("nil thread")
    continue
  }
  pthread_join(t, nil)
}

// CHECK: ThreadSanitizer: data race
