// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim %target-embedded-clock-shim -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded

import _Concurrency

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
  }
}
