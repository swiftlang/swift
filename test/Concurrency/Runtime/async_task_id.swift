// RUN: %target-run-simple-swift( -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: use_os_stdlib

@available(SwiftStdlib 6.5, *)
@main struct Main {
  static func main() async {
    // Task.currentID is non-nil inside a task
    let current = Task.currentID
    print("current id is some: \(current != nil)")
    // CHECK: current id is some: true

    // id is stable: reading twice yields the same value
    let first = Task.currentID
    let second = Task.currentID
    print("stable: \(first == second)")
    // CHECK: stable: true

    // UnsafeCurrentTask.id agrees with Task.currentID
    let matchesUnsafe: Bool = withUnsafeCurrentTask { task in
      guard let task else { return false }
      return task.id == Task.currentID
    }
    print("unsafe matches: \(matchesUnsafe)")
    // CHECK: unsafe matches: true

    // Task.ID exposes the raw 64-bit value
    let raw = Task.currentID?.rawValue ?? 0
    print("raw is non-zero: \(raw != 0)")
    // CHECK: raw is non-zero: true

    // Different child tasks have different IDs
    let t1 = Task { Task.currentID }
    let t2 = Task { Task.currentID }
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
