// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

import Dispatch

func asyncEcho(_ value: Int) async -> Int {
  value
}

func test_taskGroup_cancel_then_add() async {
  // CHECK: test_taskGroup_cancel_then_add
  print("\(#function)")
  let result: Int = await withTaskGroup(of: Int.self) { group in

    let addedFirst = group.spawn {
      while !Task.isCancelled {
        await Task.sleep(50)
      }
      print("first done")
      return 1
    }
    print("added first: \(addedFirst.successfully)") // CHECK: added first: true

    print("cancel first") // CHECK: cancel first
    addedFirst.handle?.cancel() // CHECK: first done

    let one = await group.next()!
    print("next first: \(one)") // CHECK: next first: 1

    let addedSecond = group.spawn { 2 }
    print("added second: \(addedSecond.successfully)") // CHECK: added second: true

    let none = await group.next()!
    print("next second: \(none)") // CHECK: next second: 2

    return 0
  }

  print("result: \(result)") // CHECK: result: 0
}



@main struct Main {
  static func main() async {
    await test_taskGroup_cancel_then_add()
  }
}
