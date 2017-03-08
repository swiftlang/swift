// RUN: %target-swiftc_driver %s -g -sanitize=thread -o %t_tsan-binary
// RUN: not env TSAN_OPTIONS=abort_on_error=0 %target-run %t_tsan-binary 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: CPU=x86_64
// REQUIRES: tsan_runtime
// XFAIL: linux

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

import Darwin

var threads: [pthread_t?] = []
var racey_x: Int;

for _ in 1...5 {
  var t : pthread_t?
  pthread_create(&t, nil, { _ in
    print("pthread ran")
    racey_x = 5;

    return nil
  }, nil)
  threads.append(t)
}

for t in threads {
  if t == nil {
    print("nil thread")
    continue
  }
  pthread_join(t!, nil)
}

// CHECK: ThreadSanitizer: data race
