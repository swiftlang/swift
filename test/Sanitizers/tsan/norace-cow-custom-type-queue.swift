// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/tsan_invisible_barrier.c -c -o %t/tsan_invisible_barrier.o
// RUN: %target-swiftc_driver %s %t/tsan_invisible_barrier.o -import-objc-header %S/Inputs/tsan_invisible_barrier.h -Onone -parse-as-library -g -sanitize=thread %import-libdispatch -o %t/test-Onone
// RUN: %target-codesign %t/test-Onone
// RUN: env %env-TSAN_OPTIONS=abort_on_error=0 %target-run %t/test-Onone 2>&1 | %FileCheck %s --dump-input=fail --implicit-check-not='ThreadSanitizer'
// RUN: %target-swiftc_driver %s %t/tsan_invisible_barrier.o -import-objc-header %S/Inputs/tsan_invisible_barrier.h -O -parse-as-library -g -sanitize=thread %import-libdispatch -o %t/test-O
// RUN: %target-codesign %t/test-O
// RUN: env %env-TSAN_OPTIONS=abort_on_error=0 %target-run %t/test-O 2>&1 | %FileCheck %s --dump-input=fail --implicit-check-not='ThreadSanitizer'
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

// A reader takes a copy of a copy-on-write value that a writer later mutates.
// The queue orders taking the copy, not the reader's use of it, so the only
// thing ordering the reader's reads before the in-place mutation is the copy's
// release. That must not be reported as a race.

import Dispatch

final class Storage { var value = 0 }

struct Buffer {
  private var storage = Storage()
  var value: Int { storage.value }
  mutating func set(_ v: Int) {
    if !isKnownUniquelyReferenced(&storage) { storage = Storage() }
    storage.value = v
  }
}

final class MyClass: @unchecked Sendable {
  private var buffer = Buffer()
  // We use this queue to make sure a real swift-access-race does not occur
  private let q = DispatchQueue(label: "q")

  func getCopy() -> Buffer {
    return q.sync { buffer }
  }

  func modify() {
    q.sync { buffer.set(1) }
  }
}

@main struct Test {
  static func main() {
    swift_tsan_invisible_barrier_init(2)
    let a = MyClass()
    DispatchQueue.global().async {
      print(a.getCopy().value)
      swift_tsan_invisible_barrier_wait()
    }
    swift_tsan_invisible_barrier_wait()
    a.modify()  // the reader's copy is gone: mutates in place
    print("Done.")
  }
}

// CHECK: Done.
