// RUN: %target-swiftc_driver %s -g -sanitize=thread %import-libdispatch -o %t_tsan-binary
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
  var tdo : TestDeallocObject = TestDeallocObject()
  tdo.accessMember()

  // Read the value from a different thread.
  let concurrentQueue = DispatchQueue(label: "queuename", attributes: .concurrent)
  concurrentQueue.async {
    tdo.accessMember()
  }
  // Read the value from this thread.
  tdo.accessMember()
  sleep(1)

  // Deinit the value.
}

print("Done.")

// CHECK: Done.
