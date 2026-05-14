// RUN: %target-run-simple-swift( -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: use_os_stdlib

@available(SwiftStdlib 6.5, *)
@main struct Main {
  static func main() async {
    // Task.currentIdentifier is non-nil inside a task
    let current = Task.currentIdentifier
    print("current id is some: \(current != nil)")
    // CHECK: current id is some: true

    // identifier is stable: reading twice yields the same value
    let first = Task.currentIdentifier
    let second = Task.currentIdentifier
    print("stable: \(first == second)")
    // CHECK: stable: true

    // UnsafeCurrentTask.identifier agrees with Task.currentIdentifier
    let matchesUnsafe: Bool = withUnsafeCurrentTask { task in
      guard let task else { return false }
      return task.identifier == Task.currentIdentifier
    }
    print("unsafe matches: \(matchesUnsafe)")
    // CHECK: unsafe matches: true

    // Task.Identifier exposes the raw 64-bit value
    let raw = Task.currentIdentifier?.rawValue ?? 0
    print("raw is non-zero: \(raw != 0)")
    // CHECK: raw is non-zero: true

    // Different child tasks have different identifiers
    let t1 = Task { Task.currentIdentifier }
    let t2 = Task { Task.currentIdentifier }
    let id1 = await t1.value
    let id2 = await t2.value
    print("both non-nil: \(id1 != nil && id2 != nil)")
    // CHECK: both non-nil: true
    print("different: \(id1 != id2)")
    // CHECK: different: true

    print("done")
    // CHECK: done
  }
}
