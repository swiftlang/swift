// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-clang -x c -std=c11 -I %swift_obj_root/include -c %t/time.c -o %t/time.o
// RUN: %target-clang -x c -c %S/Inputs/unbuffered-putchar.c -o %t/unbuffered-putchar.o
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library -D WALL %t/main.swift -c -o %t/time_wall.o
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library %t/main.swift -c -o %t/time_unknown.o
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library -D RESOLUTION -D WALL %t/main.swift -c -o %t/resolution_wall.o
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library -D RESOLUTION %t/main.swift -c -o %t/resolution_unknown.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/time_wall.o %t/unbuffered-putchar.o %t/time.o -o %t/time_wall.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/time_unknown.o %t/unbuffered-putchar.o %t/time.o -o %t/time_unknown.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/resolution_wall.o %t/unbuffered-putchar.o %t/time.o -o %t/resolution_wall.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/resolution_unknown.o %t/unbuffered-putchar.o %t/time.o -o %t/resolution_unknown.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-not-crash %target-run %t/time_wall.out | %FileCheck %t/main.swift
// RUN: %target-not-crash %target-run %t/time_unknown.out | %FileCheck %t/main.swift
// RUN: %target-not-crash %target-run %t/resolution_wall.out | %FileCheck %t/main.swift
// RUN: %target-not-crash %target-run %t/resolution_unknown.out | %FileCheck %t/main.swift

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_embedded_platform
// REQUIRES: swift_feature_Extern

//--- time.c
#include <swift/ExecutorImpl.h>

void test_timeNow(int clock, long long *seconds, long long *nanoseconds) {
  SwiftTime time = swift_time_now(clock);
  *seconds = time.seconds;
  *nanoseconds = time.nanoseconds;
}

void test_timeResolution(int clock, long long *seconds,
                         long long *nanoseconds) {
  SwiftTime time = swift_time_getResolution(clock);
  *seconds = time.seconds;
  *nanoseconds = time.nanoseconds;
}

//--- main.swift

// swift_time_now and swift_time_getResolution of ExecutorImpl.h, through the
// C wrappers in time.c.
@_extern(c, "test_timeNow")
func swift_time_now(
  _ clock: CInt,
  _ seconds: UnsafeMutablePointer<Int64>,
  _ nanoseconds: UnsafeMutablePointer<Int64>)

@_extern(c, "test_timeResolution")
func swift_time_getResolution(
  _ clock: CInt,
  _ seconds: UnsafeMutablePointer<Int64>,
  _ nanoseconds: UnsafeMutablePointer<Int64>)

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
    swift_time_getResolution(clock, &seconds, &nanoseconds)
#else
    swift_time_now(clock, &seconds, &nanoseconds)
#endif
    print("after the call")
  }
}

// CHECK: before the call
// CHECK-NOT: after the call
