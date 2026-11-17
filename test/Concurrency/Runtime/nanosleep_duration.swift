// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: OS=macosx || OS=linux-gnu || OS=wasip1
// UNSUPPORTED: back_deployment_runtime

// WasmKit rounds each sleep down to whole milliseconds.
// XFAIL: OS=wasip1

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#elseif canImport(WASILibc)
import WASILibc
#endif

let clock = ContinuousClock()
for duration in [0, 500_000, 999_999, 1_000_000, 1_500_000, 2_900_000] as [Int64] {
  // Host load can stretch a sleep that the host cut short.
  let shortest = (0..<5).map { _ in
    var request = timespec(tv_sec: 0, tv_nsec: .init(duration))
    return clock.measure {
      guard nanosleep(&request, nil) == 0 else {
        fatalError("nanosleep of \(duration) ns failed, errno \(errno)")
      }
    }
  }.min()!
  print(
    shortest >= .nanoseconds(duration)
      ? "\(duration) ns: ok" : "\(duration) ns: short, slept \(shortest)")
}
// CHECK: {{^}}0 ns: ok
// CHECK-NEXT: {{^}}500000 ns: ok
// CHECK-NEXT: {{^}}999999 ns: ok
// CHECK-NEXT: {{^}}1000000 ns: ok
// CHECK-NEXT: {{^}}1500000 ns: ok
// CHECK-NEXT: {{^}}2900000 ns: ok
