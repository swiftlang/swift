// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library -sanitize=thread) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch
// REQUIRES: tsan_runtime
// REQUIRES: synchronization
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: use_os_stdlib

// Bug in TSan on FreeBSD
// Thread destruction interceptor marks the thread ignored and then checks that
// the thread isn't being ignored.
// rdar://158450231
// XFAIL: OS=freebsd

// Check that TSan sees a continuation's resume happen before the await returns
// when the continuation is resumed before it's awaited.

import Dispatch
import Synchronization

final class Box: Sendable {
  let x: Int
  init(_ x: Int) { self.x = x }

  // Keeps the load in instrumented code, where TSan sees it.
  @inline(never) func getX() -> Int { x }
}

let resumed = Atomic<Bool>(false)

@main struct Main {
  static func main() async {
    let box = await withCheckedContinuation { (c: CheckedContinuation<Box, Never>) in
      DispatchQueue.global().async {
        c.resume(returning: Box(42))
        resumed.store(true, ordering: .relaxed)
      }
      // Relaxed, so it adds no happens-before edge of its own.
      while !resumed.load(ordering: .relaxed) {}
    }
    // CHECK: x: 42
    print("x: \(box.getX())")
  }
}
