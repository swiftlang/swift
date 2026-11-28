// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library %s -o %t/time.out
// RUN: %target-build-swift -parse-as-library -D RESOLUTION %s -o %t/resolution.out
// RUN: %target-codesign %t/time.out
// RUN: %target-codesign %t/resolution.out
// RUN: %target-not-crash %target-run %t/time.out 2>&1 | %FileCheck %s
// RUN: %target-not-crash %target-run %t/resolution.out 2>&1 | %FileCheck %s

// An unknown clock ID stops the process with a message that names the ID.

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

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

@main
struct Main {
  static func main() {
    var seconds: Int64 = 0
    var nanoseconds: Int64 = 0
#if RESOLUTION
    swift_get_clock_res(&seconds, &nanoseconds, 99)
#else
    swift_get_time(&seconds, &nanoseconds, 99)
#endif
    print("after the call")
  }
}

// CHECK: Fatal error: invalid clock ID 99
// CHECK-NOT: after the call
