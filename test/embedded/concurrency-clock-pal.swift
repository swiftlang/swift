// The Swift clock APIs on top of the Embedded Swift platform abstraction
// layer: ContinuousClock, SuspendingClock and Task.sleep, down to the clock
// hooks in swift/EmbeddedPlatform.h.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// (1) With a fake platform clock, the Swift clock APIs must report exactly
// the values the platform hooks return. Each clock's hooks answer with
// distinct values, so what the APIs print identifies which hook produced it.
// fake-clock.c defines every hook, so the clock shim archive that
// %target-embedded-link bundles is never drawn on.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %t/values.swift -c -o %t/values.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt -I %swift_obj_root/include -x c %t/fake-clock.c -x none %t/values.o -o %t/values.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lc++ -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/values.out | %FileCheck %t/values.swift

// (2) With the bundled POSIX clock shim, the same APIs link without a
// user-provided clock, report advancing time, and actually sleep.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %t/sleep.swift -c -o %t/sleep.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/sleep.o -o %t/sleep.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lc++ -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/sleep.out | %FileCheck %t/sleep.swift

// (3) Only the default cooperative global executor calls the sleep hook. A
// program with a custom global executor schedules delayed jobs itself, so
// Task.sleep still works with a clock whose sleep hook traps. The link line
// spells out %target-embedded-link's archives instead of using the wrapper
// so that the bundled clock shim stays out — trap-clock.c must be the only
// clock provider for the trapping sleep hook to prove anything — and the
// default executor is replaced by the custom one.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %t/executor.swift -c -o %t/executor.o
// RUN: %target-clang -x c -std=c11 -I %swift_obj_root/include -c %S/Inputs/executor.c -o %t/custom-executor.o
// RUN: %target-clang %target-clang-resource-dir-opt -I %swift_obj_root/include -x c %t/trap-clock.c -x none %t/executor.o %t/custom-executor.o -o %t/executor.out %swift_obj_root/lib/swift/embedded/%module-target-triple/libswiftCore.a %swift_obj_root/lib/swift/embedded/%module-target-triple/libswiftEmbeddedPlatformPOSIX.a %swift_obj_root/lib/swift/embedded/%module-target-triple/libswift_Concurrency.a %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/executor.out | %FileCheck %t/executor.swift

// REQUIRES: swift_embedded_platform
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

//--- values.swift

import _Concurrency

// A Duration as whole seconds plus nanoseconds, which is how the platform
// hooks report time.
func report(_ label: String, _ duration: Duration) {
  let (seconds, attoseconds) = duration.components
  print("\(label): \(seconds) \(attoseconds / 1_000_000_000)")
}

@main
struct Main {
  static func main() {
    let continuous = ContinuousClock()
    let suspending = SuspendingClock()

    // `systemEpoch` is the clock's zero instant, so the duration from it to
    // `now` is exactly what the platform hook returned.
    report("continuous now", continuous.systemEpoch.duration(to: continuous.now))
    // CHECK: continuous now: 11 22
    report("continuous resolution", continuous.minimumResolution)
    // CHECK-NEXT: continuous resolution: 0 33

    // Distinct values, so this also shows the continuous and suspending
    // entry points reach their own hooks and not each other's.
    report("suspending now", suspending.systemEpoch.duration(to: suspending.now))
    // CHECK-NEXT: suspending now: 44 55
    report("suspending resolution", suspending.minimumResolution)
    // CHECK-NEXT: suspending resolution: 0 66

    // Instant arithmetic and `measure` are plain Swift on top of `now`.
    let start = continuous.now
    report("advanced", start.duration(to: start.advanced(by: .seconds(5))))
    // CHECK-NEXT: advanced: 5 0

    // The fake clock never moves, so a measured interval is zero.
    report("measured", continuous.measure { })
    // CHECK-NEXT: measured: 0 0

    print("done")
    // CHECK-NEXT: done
  }
}

//--- sleep.swift

import _Concurrency

func elapsedMilliseconds(_ duration: Duration) -> Int64 {
  let (seconds, attoseconds) = duration.components
  return seconds * 1000 + attoseconds / 1_000_000_000_000_000
}

func check(_ label: String, _ elapsed: Duration, atLeast milliseconds: Int64) {
  print(elapsedMilliseconds(elapsed) >= milliseconds
        ? "\(label): ok" : "\(label): too short")
}

@main
struct Main {
  static func main() async {
    let continuous = ContinuousClock()

    let first = continuous.now
    let second = continuous.now
    print(first <= second ? "advancing" : "not advancing")
    // CHECK: advancing

    var start = continuous.now
    try! await Task.sleep(for: .milliseconds(50))
    check("Task.sleep(for:)", start.duration(to: continuous.now), atLeast: 50)
    // CHECK-NEXT: Task.sleep(for:): ok

    start = continuous.now
    try! await Task.sleep(until: start.advanced(by: .milliseconds(50)))
    check("Task.sleep(until:)", start.duration(to: continuous.now), atLeast: 50)
    // CHECK-NEXT: Task.sleep(until:): ok

    start = continuous.now
    try! await Task.sleep(nanoseconds: 50_000_000)
    check("Task.sleep(nanoseconds:)", start.duration(to: continuous.now),
          atLeast: 50)
    // CHECK-NEXT: Task.sleep(nanoseconds:): ok

    // The same, driven by the suspending clock rather than the continuous one.
    let suspending = SuspendingClock()
    let suspendingStart = suspending.now
    try! await suspending.sleep(
      until: suspendingStart.advanced(by: .milliseconds(50)))
    check("SuspendingClock.sleep(until:)",
          suspendingStart.duration(to: suspending.now), atLeast: 50)
    // CHECK-NEXT: SuspendingClock.sleep(until:): ok

    print("done")
    // CHECK-NEXT: done
  }
}

//--- executor.swift

import _Concurrency

@main
struct Main {
  static func main() async {
    // The custom executor schedules the delayed job off its own timer, so
    // this completes without the runtime ever calling the sleep hook.
    try! await Task.sleep(for: .milliseconds(10))
    print("slept")
    // CHECK: slept

    let task = Task { 42 }
    print(await task.value == 42 ? "ran" : "did not run")
    // CHECK: ran

    print("done")
    // CHECK: done
  }
}

//--- fake-clock.c

/* A fake platform clock. Each hook reports a fixed, distinct value, so the
   Swift clock APIs can be checked against exact numbers. */

#include <swift/EmbeddedPlatform.h>

void _swift_clockContinuous_getTime(__swift_int64_t *seconds,
                                    __swift_int64_t *nanoseconds) {
  *seconds = 11;
  *nanoseconds = 22;
}

void _swift_clockContinuous_getResolution(__swift_int64_t *seconds,
                                          __swift_int64_t *nanoseconds) {
  *seconds = 0;
  *nanoseconds = 33;
}

void _swift_clockSuspending_getTime(__swift_int64_t *seconds,
                                    __swift_int64_t *nanoseconds) {
  *seconds = 44;
  *nanoseconds = 55;
}

void _swift_clockSuspending_getResolution(__swift_int64_t *seconds,
                                          __swift_int64_t *nanoseconds) {
  *seconds = 0;
  *nanoseconds = 66;
}

void _swift_clock_sleep(__swift_int64_t seconds, __swift_int64_t nanoseconds) {
  (void)seconds;
  (void)nanoseconds;
}

//--- trap-clock.c

/* A minimal clock provider for a program with a custom global executor. The
   executor drives its timer queue from swift_time_now, so the time hooks must
   answer; the sleep hook is only ever called by the default cooperative
   global executor, so it traps. */

#include <swift/EmbeddedPlatform.h>

#include <time.h>

static void getTime(clockid_t clock, __swift_int64_t *seconds,
                    __swift_int64_t *nanoseconds) {
  struct timespec ts;
  clock_gettime(clock, &ts);
  *seconds = ts.tv_sec;
  *nanoseconds = ts.tv_nsec;
}

void _swift_clockContinuous_getTime(__swift_int64_t *seconds,
                                    __swift_int64_t *nanoseconds) {
  getTime(CLOCK_MONOTONIC_RAW, seconds, nanoseconds);
}

void _swift_clockContinuous_getResolution(__swift_int64_t *seconds,
                                          __swift_int64_t *nanoseconds) {
  *seconds = 0;
  *nanoseconds = 1;
}

void _swift_clockSuspending_getTime(__swift_int64_t *seconds,
                                    __swift_int64_t *nanoseconds) {
  getTime(CLOCK_UPTIME_RAW, seconds, nanoseconds);
}

void _swift_clockSuspending_getResolution(__swift_int64_t *seconds,
                                          __swift_int64_t *nanoseconds) {
  *seconds = 0;
  *nanoseconds = 1;
}

void _swift_clock_sleep(__swift_int64_t seconds, __swift_int64_t nanoseconds) {
  (void)seconds;
  (void)nanoseconds;
  __builtin_trap();
}
