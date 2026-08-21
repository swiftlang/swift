// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -plugin-path %swift-plugin-dir -parse-as-library %s -c -o %t/a.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lc++ -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim %target-embedded-observation -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

import Observation

@Observable
final class Counter {
  var value: Int = 0
  var untracked: Int = 0
}

@main
struct Main {
  static func main() {
    let c = Counter()

    // Mutating a property that was read inside the apply closure fires
    // onChange, synchronously, before the new value is stored.
    withObservationTracking {
      _ = c.value
    } onChange: {
      print("changed-1")
    }
    c.value = 1
    print("after-1 \(c.value)")
    // CHECK: changed-1
    // CHECK-NEXT: after-1 1

    // Only the properties actually read inside the apply closure are tracked,
    // so mutating a different property must not fire onChange.
    withObservationTracking {
      _ = c.value
    } onChange: {
      print("changed-2")
    }
    c.untracked = 1
    print("after-untracked")
    // CHECK-NEXT: after-untracked

    // ...but the still-installed observer does fire on the next change to the
    // tracked property.
    c.value = 2
    print("after-2 \(c.value)")
    // CHECK-NEXT: changed-2
    // CHECK-NEXT: after-2 2

    // Tracking is one-shot: it cancelled itself above, so a further mutation
    // fires nothing.
    c.value = 3
    print("after-3 \(c.value)")
    // CHECK-NEXT: after-3 3
  }
}
