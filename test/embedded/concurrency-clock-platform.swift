// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library %t/values.swift -c -o %t/values.o
// RUN: %target-clang -x c -std=c11 -I %swift_obj_root/include -c %t/values_clock.c -o %t/values_clock.o
// RUN: %target-clang -x c -std=c11 -I %swift_obj_root/include -c %t/time.c -o %t/time.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/values.o %t/values_clock.o %t/time.o -o %t/values.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_Concurrency %if OS=macosx %{ -lswift_ConcurrencyDefaultExecutor %} %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/values.out | %FileCheck %t/values.swift

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %t/suspending_only.swift -c -o %t/suspending_only.o
// RUN: %target-clang -x c -std=c11 -I %swift_obj_root/include -c %t/suspending_only_clock.c -o %t/suspending_only_clock.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/suspending_only.o %t/suspending_only_clock.o -o %t/suspending_only.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_Concurrency %if OS=macosx %{ -lswift_ConcurrencyDefaultExecutor %} %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/suspending_only.out | %FileCheck %t/suspending_only.swift
// RUN: %llvm-nm --defined-only --format=just-symbols %t/suspending_only.out | %FileCheck %t/suspending_only.swift --check-prefix=SYMBOLS

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded
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

//--- values_clock.c
#include <swift/EmbeddedPlatform.h>

void _swift_clockContinuous_getTime(__swift_int64_t *s, __swift_int64_t *ns) {
  *s = 11;
  *ns = 22;
}

void _swift_clockContinuous_getResolution(__swift_int64_t *s,
                                          __swift_int64_t *ns) {
  *s = 0;
  *ns = 33;
}

void _swift_clockSuspending_getTime(__swift_int64_t *s, __swift_int64_t *ns) {
  *s = 44;
  *ns = 55;
}

void _swift_clockSuspending_getResolution(__swift_int64_t *s,
                                          __swift_int64_t *ns) {
  *s = 0;
  *ns = 66;
}

//--- values.swift
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

func report(_ label: StaticString, _ duration: Duration) {
  let (seconds, attoseconds) = duration.components
  print(label, terminator: ": ")
  print(seconds, terminator: " ")
  print(attoseconds / 1_000_000_000)
}

func report(
  _ label: StaticString,
  _ read: (CInt, UnsafeMutablePointer<Int64>, UnsafeMutablePointer<Int64>) -> Void,
  _ clock: CInt
) {
  var seconds: Int64 = 0
  var nanoseconds: Int64 = 0
  read(clock, &seconds, &nanoseconds)
  print(label, terminator: ": ")
  print(seconds, terminator: " ")
  print(nanoseconds)
}

@main
struct Main {
  static func main() {
    let continuous = ContinuousClock()
    report("continuous now", continuous.systemEpoch.duration(to: continuous.now))
    // CHECK: continuous now: 11 22
    report("continuous resolution", continuous.minimumResolution)
    // CHECK-NEXT: continuous resolution: 0 33
    let suspending = SuspendingClock()
    report("suspending now", suspending.systemEpoch.duration(to: suspending.now))
    // CHECK-NEXT: suspending now: 44 55
    report("suspending resolution", suspending.minimumResolution)
    // CHECK-NEXT: suspending resolution: 0 66

    report("swift_time_now continuous", swift_time_now, continuousClockID)
    // CHECK-NEXT: swift_time_now continuous: 11 22
    report("swift_time_getResolution continuous", swift_time_getResolution, continuousClockID)
    // CHECK-NEXT: swift_time_getResolution continuous: 0 33
    report("swift_time_now suspending", swift_time_now, suspendingClockID)
    // CHECK-NEXT: swift_time_now suspending: 44 55
    report("swift_time_getResolution suspending", swift_time_getResolution, suspendingClockID)
    // CHECK-NEXT: swift_time_getResolution suspending: 0 66
  }
}

//--- suspending_only_clock.c
#include <swift/EmbeddedPlatform.h>

void _swift_clockSuspending_getTime(__swift_int64_t *s, __swift_int64_t *ns) {
  *s = 44;
  *ns = 0;
}

//--- suspending_only.swift
import _Concurrency

@main
struct Main {
  static func main() {
    let suspending = SuspendingClock()
    let elapsed = suspending.systemEpoch.duration(to: suspending.now)
    print(elapsed == .seconds(44) ? "suspending only: ok" : "suspending only: wrong")
    // CHECK: suspending only: ok
    // SYMBOLS-NOT: _swift_clockContinuous_
    // SYMBOLS-NOT: _swift_clockSuspending_getResolution
  }
}
