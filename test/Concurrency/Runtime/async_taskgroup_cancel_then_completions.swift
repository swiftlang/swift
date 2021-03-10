// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

import Dispatch

func asyncEcho(_ value: Int) async -> Int {
  value
}

func test_taskGroup_cancel_then_completions() async {
  // CHECK: test_taskGroup_cancel_then_completions
  print("before \(#function)")

  let result: Int = try! await Task.withGroup(resultType: (Int, Bool).self) { group in
    print("group cancelled: \(group.isCancelled)") // CHECK: group cancelled: false
    let addedFirst = await group.add {
      print("start first")
      await Task.sleep(1_000_000_000)
      print("done first")
      return (1, Task.isCancelled)
    }
    print("added first: \(addedFirst)") // CHECK: added first: true
    assert(addedFirst)

    let addedSecond = await group.add {
      print("start second")
      await Task.sleep(3_000_000_000)
      print("done second")
      return (2, Task.isCancelled)
    }
    print("added second: \(addedSecond)") // CHECK: added second: true
    assert(addedSecond)

    group.cancelAll() // FIXME: dont make it async
    print("cancelAll") // CHECK: cancelAll

//    let outerCancelled = await outer // should not be cancelled
//    print("outer cancelled: \(outerCancelled)") // COM: CHECK: outer cancelled: false
//    print("group cancelled: \(group.isCancelled)") // COM: CHECK: outer cancelled: false

    let one = try! await group.next()
    print("first: \(one)") // CHECK: first: Optional((1,
    let two = try! await group.next()
    print("second: \(two)") // CHECK: second: Optional((2,
    let none = try! await group.next()
    print("none: \(none)") // CHECK: none: nil

    return (one?.0 ?? 0) + (two?.0 ?? 0) + (none?.0 ?? 0)
  }

  print("result: \(result)") // CHECK: result: 3
}

@main struct Main {
  static func main() async {
    await test_taskGroup_cancel_then_completions()
  }
}
