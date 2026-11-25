// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-cxx-lib -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim %if OS=linux-gnu %{ -Wl,--gc-sections %} %else %{ -dead_strip %}
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu || OS=wasip1
// REQUIRES: swift_feature_Embedded

import _Concurrency

@MainActor var counter = 0

@MainActor func bump() -> Int {
  counter += 1
  return counter
}

func assumeMainActor(_ message: StaticString) {
  MainActor.assumeIsolated { print(message) }
}

@main
struct Main {
  static func main() async {
    // bump() without await compiles only while main() is isolated to the main actor.
    print("main: \(bump())")
    // CHECK: main: 1
    assumeMainActor("main: assumed")
    // CHECK-NEXT: main: assumed
    let fromDetached = await Task.detached { await bump() }.value
    print("detached: \(fromDetached)")
    // CHECK-NEXT: detached: 2
    await Task { @MainActor in assumeMainActor("main-actor task: assumed") }.value
    // CHECK-NEXT: main-actor task: assumed
    await MainActor.run { assumeMainActor("MainActor.run: assumed") }
    // CHECK-NEXT: MainActor.run: assumed
  }
}
