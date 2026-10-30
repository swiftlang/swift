// RUN: %empty-directory(%t)
// RUN: %target-clang -x c -c %S/Inputs/unbuffered-putchar.c -o %t/unbuffered-putchar.o
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -D WALL %s -c -o %t/time_wall.o
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/time_unknown.o
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -D RESOLUTION -D WALL %s -c -o %t/resolution_wall.o
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -D RESOLUTION %s -c -o %t/resolution_unknown.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/time_wall.o %t/unbuffered-putchar.o -o %t/time_wall.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/time_unknown.o %t/unbuffered-putchar.o -o %t/time_unknown.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/resolution_wall.o %t/unbuffered-putchar.o -o %t/resolution_wall.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/resolution_unknown.o %t/unbuffered-putchar.o -o %t/resolution_unknown.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-not-crash %target-run %t/time_wall.out | %FileCheck %s
// RUN: %target-not-crash %target-run %t/time_unknown.out | %FileCheck %s
// RUN: %target-not-crash %target-run %t/resolution_wall.out | %FileCheck %s
// RUN: %target-not-crash %target-run %t/resolution_unknown.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded

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

let wallClockID: CInt = 3
let unknownClockID: CInt = 99

@main
struct Main {
  static func main() {
#if WALL
    let clock = wallClockID
#else
    let clock = unknownClockID
#endif
    var seconds: Int64 = 0
    var nanoseconds: Int64 = 0
    print("before the call")
#if RESOLUTION
    swift_get_clock_res(&seconds, &nanoseconds, clock)
#else
    swift_get_time(&seconds, &nanoseconds, clock)
#endif
    print("after the call")
  }
}

// CHECK: before the call
// CHECK-NOT: after the call
