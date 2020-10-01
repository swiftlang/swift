// RUN: %target-swiftc_driver %s -g -sanitize=thread %import-libdispatch -o %t_tsan-binary
// RUN: %target-codesign %t_tsan-binary
// RUN: env %env-TSAN_OPTIONS=abort_on_error=0 not %target-run %t_tsan-binary 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: tsan_runtime
// UNSUPPORTED: OS=tvos

// FIXME: This should be covered by "tsan_runtime"; older versions of Apple OSs
// don't support TSan.
// UNSUPPORTED: remote_run

// Test ThreadSanitizer execution end-to-end with libdispatch.

import Dispatch

let q = DispatchQueue(label: "q", attributes: .concurrent)

let sync1 = DispatchSemaphore(value: 0)
let sync2 = DispatchSemaphore(value: 0)
let finish = DispatchSemaphore(value: 0)

func wait1_signal2() { sync1.wait();   sync2.signal() }
func signal1_wait2() { sync1.signal(); sync2.wait()   }

func race() {
  var racy = 1

  q.async {
    wait1_signal2()
    racy = 2
    wait1_signal2()
    finish.signal()
  }
  q.async {
    signal1_wait2()
    racy = 3
    signal1_wait2()
    finish.signal()
  }

  finish.wait()
  finish.wait()
}

// TSan %deflake as part of the test.
for _ in 1...10 {
  race()
}

print("Done!")

// CHECK: ThreadSanitizer: data race
// CHECK: Done!
