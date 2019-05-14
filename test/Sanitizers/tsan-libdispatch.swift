// RUN: %target-swiftc_driver %s -g -sanitize=thread %import-libdispatch -o %t_tsan-binary
// RUN: %target-codesign %t_tsan-binary
// RUN: not env %env-TSAN_OPTIONS=abort_on_error=0 %target-run %t_tsan-binary 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: tsan_runtime
// UNSUPPORTED: OS=tvos

// FIXME: This should be covered by "tsan_runtime"; older versions of Apple OSs
// don't support TSan.
// UNSUPPORTED: remote_run

// Test ThreadSanitizer execution end-to-end with libdispatch.

import Dispatch

let sync1 = DispatchSemaphore(value: 0)
let sync2 = DispatchSemaphore(value: 0)
let finish = DispatchSemaphore(value: 0)

let q = DispatchQueue(label: "q", attributes: .concurrent)

var racy = 1

q.async {
  sync1.wait()
  sync2.signal()
  racy = 2
  finish.signal()
}
q.async {
  sync1.signal()
  sync2.wait()
  racy = 3
  finish.signal()
}

finish.wait()
finish.wait()

print("Done!")

// CHECK: ThreadSanitizer: data race
// CHECK: Done!
