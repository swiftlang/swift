// Verify that the Concurrency runtime routes clock queries through the
// Embedded Swift platform abstraction layer clock hooks.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %t/main.swift -c -o %t/main.o

// (1) With a fake platform clock, the runtime must report exactly the values
// the platform hooks return, passing each clock ID and the sleep duration
// through unchanged. The fake provider is an explicit object on the link
// line, so it takes precedence over the bundled clock shim archive.
// RUN: %target-embedded-link %target-clang-resource-dir-opt -x c %t/fake-clock.c -x none %t/main.o -o %t/fake.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lc++ -lswift_Concurrency %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/fake.out | %FileCheck %t/main.swift --check-prefixes=CHECK,FAKE

// (2) With the bundled POSIX clock shim, the same program links without a
// user-provided clock and reports monotonically advancing time.
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o -o %t/real.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lc++ -lswift_Concurrency %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/real.out | %FileCheck %t/main.swift --check-prefix=CHECK

// (3) A program with a custom global executor does not need the bundled
// POSIX shim or a working sleep: this executor consults swift_time_now to
// drive its timer queue, so the provider must answer getTime, but only the
// default cooperative global executor calls swift_sleep, so the sleep hook
// may trap as long as it is linkable. The link line deliberately spells out
// the archives instead of using %target-embedded-link so that the bundled
// clock shim is not linked; trap-clock.c stands in for a platform's own
// clock hooks.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %t/noclock.swift -c -o %t/noclock.o
// RUN: %target-clang -x c -std=c11 -I %swift_obj_root/include -c %S/Inputs/executor.c -o %t/executor.o
// RUN: %target-clang %target-clang-resource-dir-opt -x c %t/trap-clock.c -x none %t/noclock.o %t/executor.o -o %t/noclock.out %swift_obj_root/lib/swift/embedded/%module-target-triple/libswiftCore.a %swift_obj_root/lib/swift/embedded/%module-target-triple/libswiftEmbeddedPlatformPOSIX.a %swift_obj_root/lib/swift/embedded/%module-target-triple/libswift_Concurrency.a %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/noclock.out | %FileCheck %t/noclock.swift --check-prefix=NOCLOCK

// REQUIRES: swift_embedded_platform
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

//--- main.swift

@_silgen_name("swift_get_time")
func swift_get_time(
  _ seconds: UnsafeMutablePointer<Int64>,
  _ nanoseconds: UnsafeMutablePointer<Int64>,
  _ clock_id: CInt)

@_silgen_name("swift_get_clock_res")
func swift_get_clock_res(
  _ seconds: UnsafeMutablePointer<Int64>,
  _ nanoseconds: UnsafeMutablePointer<Int64>,
  _ clock_id: CInt)

@_silgen_name("swift_sleep")
func swift_sleep(_ seconds: Int64, _ nanoseconds: Int64)

func getTime(_ clockID: CInt) -> (Int64, Int64) {
  var s: Int64 = 0
  var ns: Int64 = 0
  swift_get_time(&s, &ns, clockID)
  return (s, ns)
}

@main
struct Main {
  static func main() {
    print("begin")
    // CHECK: begin

    // The fake clock reports the clock ID as the seconds value and a call
    // counter as the nanoseconds value, so these lines verify that each
    // clock ID reaches the platform hook unchanged.
    let (c1s, c1n) = getTime(1) // continuous
    let (c2s, c2n) = getTime(1)
    print("t1: \(Int(c1s)) \(Int(c1n))")
    print("t2: \(Int(c2s)) \(Int(c2n))")
    // FAKE: t1: 1 1
    // FAKE: t2: 1 2

    let monotonic = c2s > c1s || (c2s == c1s && c2n >= c1n)
    print("monotonic: \(monotonic ? "yes" : "no")")
    // CHECK: monotonic: yes

    let (ss, sn) = getTime(2) // suspending
    print("suspending: \(Int(ss)) \(Int(sn))")
    // FAKE: suspending: 2 3

    let (ws, wn) = getTime(3) // wall
    print("wall: \(Int(ws)) \(Int(wn))")
    // FAKE: wall: 3 4

    var rs: Int64 = -1
    var rns: Int64 = -1
    swift_get_clock_res(&rs, &rns, 1)
    print("res: \(Int(rs)) \(Int(rns))")
    // FAKE: res: 0 1

    // The fake sleep prints its arguments; the bundled shim sleeps 5ms.
    swift_sleep(0, 5_000_000)
    // FAKE: sleep: 0 5000000

    print("end")
    // CHECK: end
  }
}

//--- noclock.swift

import _Concurrency

@main
struct Main {
  static func main() async {
    print("begin")
    // NOCLOCK: begin
    let t = Task { 42 }
    let v = await t.value
    print(v == 42 ? "ok" : "bad")
    // NOCLOCK: ok
    print("end")
    // NOCLOCK: end
  }
}

//--- fake-clock.c

#include <stdio.h>

static long long ticks = 0;

void _swift_clock_getTime(int clock_id, long long *seconds,
                          long long *nanoseconds) {
  ticks += 1;
  *seconds = clock_id;
  *nanoseconds = ticks;
}

void _swift_clock_getResolution(int clock_id, long long *seconds,
                                long long *nanoseconds) {
  *seconds = 0;
  *nanoseconds = clock_id;
}

void _swift_clock_sleep(long long seconds, long long nanoseconds) {
  printf("sleep: %lld %lld\n", seconds, nanoseconds);
}

//--- trap-clock.c

/* A minimal clock provider for a program with a custom global executor. The
   executor uses swift_time_now, so getTime must answer; swift_sleep is only
   called by the default cooperative global executor, so the sleep hook may
   trap. */

static long long ticks = 0;

void _swift_clock_getTime(int clock_id, long long *seconds,
                          long long *nanoseconds) {
  (void)clock_id;
  *seconds = 0;
  *nanoseconds = ++ticks;
}

void _swift_clock_getResolution(int clock_id, long long *seconds,
                                long long *nanoseconds) {
  (void)clock_id;
  *seconds = 0;
  *nanoseconds = 0;
}

void _swift_clock_sleep(long long seconds, long long nanoseconds) {
  (void)seconds;
  (void)nanoseconds;
  __builtin_trap();
}
