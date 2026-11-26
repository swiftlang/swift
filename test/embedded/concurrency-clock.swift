// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library %t/main.swift -c -o %t/a.o
// RUN: %target-clang -x c -std=c11 -I %swift_obj_root/include -c %t/time.c -o %t/time.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o %t/time.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %t/main.swift

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
import _Concurrency

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

let continuousClockID: CInt = 1
let suspendingClockID: CInt = 2

func runtimeNow(_ clock: CInt) -> Int64 {
  var seconds: Int64 = 0
  var nanoseconds: Int64 = 0
  swift_time_now(clock, &seconds, &nanoseconds)
  return seconds * 1_000_000_000 + nanoseconds
}

func runtimeResolution(_ clock: CInt) -> Int64 {
  var seconds: Int64 = 0
  var nanoseconds: Int64 = 0
  swift_time_getResolution(clock, &seconds, &nanoseconds)
  return seconds * 1_000_000_000 + nanoseconds
}

func advances<T: Comparable>(_ read: () -> T) -> Bool {
  let start = read()
  for _ in 0..<1_000_000 {
    if read() > start { return true }
  }
  return false
}

@main
struct Main {
  static func main() {
    let continuous = ContinuousClock()
    print(advances { continuous.now } ? "continuous advances" : "continuous stuck")
    // CHECK: continuous advances
    print(continuous.minimumResolution > .zero ? "continuous resolution positive" : "continuous resolution zero")
    // CHECK-NEXT: continuous resolution positive

    let suspending = SuspendingClock()
    print(advances { suspending.now } ? "suspending advances" : "suspending stuck")
    // CHECK-NEXT: suspending advances
    print(suspending.minimumResolution > .zero ? "suspending resolution positive" : "suspending resolution zero")
    // CHECK-NEXT: suspending resolution positive

    let start = ContinuousClock.Instant.now
    print(start.advanced(by: .seconds(5)) - start == .seconds(5) ? "instant arithmetic: ok" : "instant arithmetic: wrong")
    // CHECK-NEXT: instant arithmetic: ok

    print(advances { runtimeNow(continuousClockID) } ? "swift_time_now continuous advances" : "swift_time_now continuous stuck")
    // CHECK-NEXT: swift_time_now continuous advances
    print(runtimeResolution(continuousClockID) > 0 ? "swift_time_getResolution continuous positive" : "swift_time_getResolution continuous zero")
    // CHECK-NEXT: swift_time_getResolution continuous positive
    print(advances { runtimeNow(suspendingClockID) } ? "swift_time_now suspending advances" : "swift_time_now suspending stuck")
    // CHECK-NEXT: swift_time_now suspending advances
    print(runtimeResolution(suspendingClockID) > 0 ? "swift_time_getResolution suspending positive" : "swift_time_getResolution suspending zero")
    // CHECK-NEXT: swift_time_getResolution suspending positive
  }
}
