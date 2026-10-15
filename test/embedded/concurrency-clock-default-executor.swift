// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %t/main.swift -c -o %t/main.o
// RUN: %target-clang -x c -std=c11 -I %swift_obj_root/include -c %t/clock.c -o %t/clock.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o %t/clock.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_Concurrency -lswift_ConcurrencyDefaultExecutor %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %t/main.swift

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

//--- clock.c
#include <swift/EmbeddedPlatform.h>

void _swift_clockSuspending_getTime(__swift_int64_t *s, __swift_int64_t *ns) {
  *s = 0;
  *ns = 0;
}

void _swift_clock_sleep(__swift_int64_t s, __swift_int64_t ns) {}

//--- main.swift
import _Concurrency

@main
struct Main {
  static func main() async {
    let task = Task { 42 }
    print(await task.value == 42 ? "async: ok" : "async: wrong")
    // CHECK: async: ok
  }
}
