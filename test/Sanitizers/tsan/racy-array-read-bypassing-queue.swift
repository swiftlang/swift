// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/tsan_invisible_barrier.c -c -o %t/tsan_invisible_barrier.o
// RUN: %target-swiftc_driver %s %t/tsan_invisible_barrier.o -import-objc-header %S/Inputs/tsan_invisible_barrier.h -Onone -parse-as-library -g -sanitize=thread %import-libdispatch -o %t/test-Onone
// RUN: %target-codesign %t/test-Onone
// RUN: env %env-TSAN_OPTIONS=abort_on_error=0 not %target-run %t/test-Onone 2>&1 | %FileCheck %s --dump-input=fail
// RUN: %target-swiftc_driver %s %t/tsan_invisible_barrier.o -import-objc-header %S/Inputs/tsan_invisible_barrier.h -O -parse-as-library -g -sanitize=thread %import-libdispatch -o %t/test-O
// RUN: %target-codesign %t/test-O
// RUN: env %env-TSAN_OPTIONS=abort_on_error=0 not %target-run %t/test-O 2>&1 | %FileCheck %s --dump-input=fail
// REQUIRES: executable_test
// REQUIRES: tsan_runtime
// REQUIRES: libdispatch

// Not verified on iOS.
// UNSUPPORTED: OS=ios

// tsan_runtime doesn't say whether a remote device's OS supports TSan.
// UNSUPPORTED: remote_run

// A read that bypasses the queue must be reported as a race with the
// queued append.

import Dispatch

final class MyClass: @unchecked Sendable {
  var array = Array(repeating: 1, count: 5)
  private let q = DispatchQueue(label: "q")

  func modifyArray() {
    q.sync { array.append(1) }
  }
}

@main struct Test {
  static func main() {
    swift_tsan_invisible_barrier_init(2)
    let a = MyClass()
    DispatchQueue.global().async {
      print(a.array[0])  // not through the queue
      swift_tsan_invisible_barrier_wait()
    }
    swift_tsan_invisible_barrier_wait()
    a.modifyArray()
    print("Done.")
  }
}

// CHECK: WARNING: ThreadSanitizer: Swift access race
// CHECK: Modifying access of Swift variable
// CHECK: Previous read
// CHECK: Done.
