// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library -sanitize=thread)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch
// REQUIRES: tsan_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: use_os_stdlib

// Resuming a continuation happens-before the code after its await, also when
// the resume comes before the task reaches the await. Then the task doesn't
// suspend: it carries on without going through an executor, so the runtime
// has to tell TSan about the synchronization itself.

import Dispatch

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#elseif canImport(Musl)
import Musl
#elseif canImport(Android)
import Android
#endif

final class Box: @unchecked Sendable {
  var value = 0
}

@main
struct ContinuationResumedBeforeAwait {
  static func main() async {
    for i in 0 ..< 20 {
      let box = Box()
      await withUnsafeContinuation { (c: UnsafeContinuation<Void, Never>) in
        DispatchQueue.global().async {
          box.value = i
          c.resume()
        }
        // Give the resume time to land before the task awaits. Sleeping is
        // not synchronization as far as TSan is concerned.
        usleep(20_000)
      }
      precondition(box.value == i)
    }
  }
}
