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

// FIXME: this is a workaround since (A, B) today isn't inferred to be Sendable
//        and causes an error, but should be a warning (this year at least)
@available(SwiftStdlib 5.1, *)
struct SendableTuple2<A: Sendable, B: Sendable>: Sendable {
  let first: A
  let second: B

  init(_ first: A, _ second: B) {
    self.first = first
    self.second = second
  }
}

@available(SwiftStdlib 5.1, *)
func test_taskGroup_cancel_then_completions() async {
  // CHECK: test_taskGroup_cancel_then_completions
  print("before \(#function)")

  let result: Int = await withTaskGroup(of: SendableTuple2<Int, Bool>.self) { group in
    print("group cancelled: \(group.isCancelled)") // CHECK: group cancelled: false
    let spawnedFirst = group.spawnUnlessCancelled {
      print("start first")
      await Task.sleep(1_000_000_000)
      print("done first")
      return SendableTuple2(1, Task.isCancelled)
    }
    print("spawned first: \(spawnedFirst)") // CHECK: spawned first: true
    assert(spawnedFirst)

    let spawnedSecond = group.spawnUnlessCancelled {
      print("start second")
      await Task.sleep(3_000_000_000)
      print("done second")
      return SendableTuple2(2, Task.isCancelled)
    }
    print("spawned second: \(spawnedSecond)") // CHECK: spawned second: true
    assert(spawnedSecond)

    group.cancelAll()
    print("cancelAll") // CHECK: cancelAll

//    let outerCancelled = await outer // should not be cancelled
//    print("outer cancelled: \(outerCancelled)") // COM: CHECK: outer cancelled: false
//    print("group cancelled: \(group.isCancelled)") // COM: CHECK: outer cancelled: false

    let one = await group.next()
    print("first: \(one)") // CHECK: first: Optional(main.SendableTuple2<Swift.Int, Swift.Bool>(first: 1,
    let two = await group.next()
    print("second: \(two)") // CHECK: second: Optional(main.SendableTuple2<Swift.Int, Swift.Bool>(first: 2,
    let none = await group.next()
    print("none: \(none)") // CHECK: none: nil

    return (one?.first ?? 0) + (two?.first ?? 0) + (none?.first ?? 0)
  }

  print("result: \(result)") // CHECK: result: 3
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await test_taskGroup_cancel_then_completions()
  }
}
