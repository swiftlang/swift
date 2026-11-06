// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/tsan_invisible_barrier.c -c -o %t/tsan_invisible_barrier.o
// RUN: %target-swiftc_driver %s %t/tsan_invisible_barrier.o -import-objc-header %S/Inputs/tsan_invisible_barrier.h -O -parse-as-library -g -sanitize=thread %import-libdispatch -o %t/test
// RUN: %target-codesign %t/test
// RUN: env %env-TSAN_OPTIONS=abort_on_error=0 %target-run %t/test 2>&1 | %FileCheck %s --dump-input=fail --implicit-check-not='ThreadSanitizer'
// REQUIRES: executable_test
// REQUIRES: tsan_runtime
// REQUIRES: libdispatch

// Not verified on iOS.
// UNSUPPORTED: OS=ios

// tsan_runtime doesn't say whether a remote device's OS supports TSan.
// UNSUPPORTED: remote_run

// The runtime must publish strong releases to TSan; an OS or back-deployed
// runtime that predates that does not.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// A reader takes a copy of an array that a writer later appends to. The queue
// orders taking the copy, not the reader's use of it, so the only thing ordering
// the reader's reads before the in-place append is the copy's release. That must
// not be reported as a race.

import Dispatch

final class MyClass: @unchecked Sendable {
  private var array = Array(repeating: 1, count: 5)
  private let q = DispatchQueue(label: "q")

  func getCopyOfArray() -> [Int] {
    return q.sync { array }
  }

  func modifyArray() {
    q.sync { array.append(1) }
  }
}

func sum(_ x: [Int]) -> Int {
  var s = 0
  for i in x { s += i }
  return s
}

@main struct Test {
  static func main() {
    swift_tsan_invisible_barrier_init(2)
    let a = MyClass()
    a.modifyArray()  // gives the array its own, uniquely referenced buffer
    DispatchQueue.global().async {
      print(sum(a.getCopyOfArray()))
      swift_tsan_invisible_barrier_wait()
    }
    swift_tsan_invisible_barrier_wait()
    a.modifyArray()  // the reader's copy is gone: appends in place
    print("Done.")
  }
}

// CHECK: Done.
