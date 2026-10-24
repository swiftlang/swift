// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %t/values.swift -c -o %t/values.o
// RUN: %target-clang -x c -std=c11 -I %swift_obj_root/include -c %t/values_clock.c -o %t/values_clock.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/values.o %t/values_clock.o -o %t/values.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_Concurrency %if OS=macosx %{ -lswift_ConcurrencyDefaultExecutor %} %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/values.out | %FileCheck %t/values.swift

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %t/suspending_only.swift -c -o %t/suspending_only.o
// RUN: %target-clang -x c -std=c11 -I %swift_obj_root/include -c %t/suspending_only_clock.c -o %t/suspending_only_clock.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/suspending_only.o %t/suspending_only_clock.o -o %t/suspending_only.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_Concurrency %if OS=macosx %{ -lswift_ConcurrencyDefaultExecutor %} %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/suspending_only.out | %FileCheck %t/suspending_only.swift

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded

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

func report(_ label: StaticString, _ duration: Duration) {
  let (seconds, attoseconds) = duration.components
  print(label, terminator: ": ")
  print(seconds, terminator: " ")
  print(attoseconds / 1_000_000_000)
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
  }
}
