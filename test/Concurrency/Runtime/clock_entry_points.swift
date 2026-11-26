// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// Checks swift_get_time, swift_get_clock_res and swift_sleep against the C
// library clocks.

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: OS=macosx || OS=linux-gnu

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

@_silgen_name("swift_get_time")
func swift_get_time(
  _ seconds: UnsafeMutablePointer<Int64>,
  _ nanoseconds: UnsafeMutablePointer<Int64>,
  _ clock: CInt)

@_silgen_name("swift_get_clock_res")
func swift_get_clock_res(
  _ seconds: UnsafeMutablePointer<Int64>,
  _ nanoseconds: UnsafeMutablePointer<Int64>,
  _ clock: CInt)

@_silgen_name("swift_sleep")
func swift_sleep(_ seconds: Int64, _ nanoseconds: Int64)

func systemNanoseconds(_ clock: clockid_t) -> Int64 {
  var time = timespec()
  clock_gettime(clock, &time)
  return Int64(time.tv_sec) * 1_000_000_000 + Int64(time.tv_nsec)
}

func runtimeNanoseconds(_ clock: CInt) -> Int64 {
  var seconds: Int64 = 0
  var nanoseconds: Int64 = 0
  swift_get_time(&seconds, &nanoseconds, clock)
  return seconds * 1_000_000_000 + nanoseconds
}

// The runtime clock ID, the system clock of its time, and the system clock of
// its resolution.
#if canImport(Darwin)
let clocks: [(id: CInt, time: clockid_t, resolution: clockid_t, name: String)] = [
  (1, CLOCK_MONOTONIC_RAW, CLOCK_MONOTONIC_RAW, "continuous"),
  (2, CLOCK_UPTIME_RAW, CLOCK_UPTIME_RAW, "suspending"),
  (3, CLOCK_REALTIME, CLOCK_REALTIME, "wall"),
]
#else
let clocks: [(id: CInt, time: clockid_t, resolution: clockid_t, name: String)] = [
  (1, CLOCK_BOOTTIME, CLOCK_BOOTTIME, "continuous"),
  (2, CLOCK_MONOTONIC, CLOCK_MONOTONIC_RAW, "suspending"),
  (3, CLOCK_REALTIME, CLOCK_REALTIME, "wall"),
]
#endif

@main
struct Main {
  static func main() {
    for clock in clocks {
      let before = systemNanoseconds(clock.time)
      let value = runtimeNanoseconds(clock.id)
      let after = systemNanoseconds(clock.time)
      print("\(clock.name) time: \(before <= value && value <= after)")

      var seconds: Int64 = -1
      var nanoseconds: Int64 = -1
      swift_get_clock_res(&seconds, &nanoseconds, clock.id)
      var expected = timespec()
      clock_getres(clock.resolution, &expected)
      let same = seconds == Int64(expected.tv_sec)
        && nanoseconds == Int64(expected.tv_nsec)
      print("\(clock.name) resolution: \(same)")
    }
    // CHECK: continuous time: true
    // CHECK-NEXT: continuous resolution: true
    // CHECK-NEXT: suspending time: true
    // CHECK-NEXT: suspending resolution: true
    // CHECK-NEXT: wall time: true
    // CHECK-NEXT: wall resolution: true

    let start = runtimeNanoseconds(1)
    swift_sleep(0, 20_000_000)
    print("sleep: \(runtimeNanoseconds(1) - start >= 20_000_000)")
    // CHECK-NEXT: sleep: true
  }
}
