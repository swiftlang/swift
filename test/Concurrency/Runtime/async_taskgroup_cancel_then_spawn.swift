// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Dispatch

@available(SwiftStdlib 5.1, *)
func asyncEcho(_ value: Int) async -> Int {
  value
}

@available(SwiftStdlib 5.1, *)
func test_taskGroup_cancel_then_add() async {
  // CHECK: test_taskGroup_cancel_then_add
  print("\(#function)")
  let result: Int = await withTaskGroup(of: Int.self) { group in

    let addedFirst = group.spawnUnlessCancelled { 1 }
    print("added first: \(addedFirst)") // CHECK: added first: true

    let one = await group.next()!
    print("next first: \(one)") // CHECK: next first: 1

    group.cancelAll()
    print("cancelAll")
    print("group isCancelled: \(group.isCancelled)") // CHECK: group isCancelled: true

    let addedSecond = group.spawnUnlessCancelled { 2 }
    print("added second: \(addedSecond)") // CHECK: added second: false

    let none = await group.next()
    print("next second: \(none)") // CHECK: next second: nil

    group.spawn {
      print("child task isCancelled: \(Task.isCancelled)") // CHECK: child task isCancelled: true
      return 3
    }
    let three = await group.next()!
    print("next third: \(three)") // CHECK: next third: 3

    print("added third, unconditionally") // CHECK: added third, unconditionally
    print("group isCancelled: \(group.isCancelled)") // CHECK: group isCancelled: true

    return one + (none ?? 0)
  }

  print("result: \(result)") // CHECK: result: 1
}



@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await test_taskGroup_cancel_then_add()
  }
}
