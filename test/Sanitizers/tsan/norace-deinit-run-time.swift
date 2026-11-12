// RUN: %empty-directory(%t)
// RUN: %target-clang -x c %S/Inputs/tsan_invisible_barrier.c -c -o %t/tsan_invisible_barrier.o
// RUN: %target-swiftc_driver %s %t/tsan_invisible_barrier.o -import-objc-header %S/Inputs/tsan_invisible_barrier.h -g -sanitize=thread %import-libdispatch -o %t_tsan-binary
// RUN: %target-codesign %t_tsan-binary
// RUN: env %env-TSAN_OPTIONS=abort_on_error=0 %target-run %t_tsan-binary 2>&1 | %FileCheck %s --dump-input=fail --implicit-check-not='ThreadSanitizer'
// REQUIRES: executable_test
// REQUIRES: tsan_runtime

// rdar://101876380
// UNSUPPORTED: OS=ios

// FIXME: This should be covered by "tsan_runtime"; older versions of Apple OSs
// don't support TSan.
// UNSUPPORTED: remote_run

// Test that we do not report a race on deinit; the synchronization is guaranteed by runtime.
import Dispatch
#if canImport(Darwin)
  import Darwin
#elseif canImport(Glibc)
  import Glibc
#else
#error("Unsupported platform")
#endif

public class TestDeallocObject {
  public var v : Int
  public init() {
    v = 1
  }

  @_optimize(none)
  func unoptimize(_ input : Int) -> Int {
    return input
  }

  func accessMember() {
    var local : Int = unoptimize(v)
    local += 1
  }

  deinit {
    v = 0
  }
}

do {
  swift_tsan_invisible_barrier_init(2)
  var tdo : TestDeallocObject = TestDeallocObject()
  tdo.accessMember()

  // Read the value from a different thread. Use Unmanaged so the worker
  // releases its +1 reference before the barrier (a normal capture would only
  // release when the closure context is destroyed after the barrier), ensuring
  // the main thread drops the last reference and runs deinit.
  let concurrentQueue = DispatchQueue(label: "queuename", attributes: .concurrent)
  let ref = Unmanaged.passRetained(tdo)
  concurrentQueue.async {
    ref.takeRetainedValue().accessMember()
    swift_tsan_invisible_barrier_wait()
  }
  // Read the value from this thread.
  tdo.accessMember()
  swift_tsan_invisible_barrier_wait()

  // Deinit the value.
}

print("Done.")

// CHECK: Done.
