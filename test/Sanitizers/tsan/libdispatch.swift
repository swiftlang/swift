// RUN: %empty-directory(%t)
// RUN: %target-clang -x c %S/Inputs/tsan_invisible_barrier.c -c -o %t/tsan_invisible_barrier.o
// RUN: %target-swiftc_driver %s %t/tsan_invisible_barrier.o -import-objc-header %S/Inputs/tsan_invisible_barrier.h -g -sanitize=thread %import-libdispatch -o %t_tsan-binary
// RUN: %target-codesign %t_tsan-binary
// RUN: env %env-TSAN_OPTIONS=abort_on_error=0 not %target-run %t_tsan-binary 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: tsan_runtime
// UNSUPPORTED: OS=tvos

// rdar://101876380
// UNSUPPORTED: OS=ios

// FIXME: This should be covered by "tsan_runtime"; older versions of Apple OSs
// don't support TSan.
// UNSUPPORTED: remote_run

// Test ThreadSanitizer execution end-to-end with libdispatch.

import Dispatch

let q = DispatchQueue(label: "q", attributes: .concurrent)

let finish = DispatchSemaphore(value: 0)

func race() {
  var racy = 1
  swift_tsan_invisible_barrier_init(2)

  q.async {
    racy = 2
    swift_tsan_invisible_barrier_wait()
    finish.signal()
  }
  q.async {
    swift_tsan_invisible_barrier_wait()
    racy = 3
    finish.signal()
  }

  finish.wait()
  finish.wait()
}

race()

print("Done!")

// CHECK-DAG: ThreadSanitizer: data race
// CHECK-DAG: Done!
