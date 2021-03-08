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
  let result: Int = try! await Task.withGroup(resultType: Int.self) { group in

    let addedFirst = await group.add { 1 }
    print("added first: \(addedFirst)") // CHECK: added first: true

    let one = try! await group.next()!
    print("next first: \(one)") // CHECK: next first: 1

    group.cancelAll()
    print("cancelAll")

    let addedSecond = await group.add { 1 }
    print("added second: \(addedSecond)") // CHECK: added second: false

    let none = try! await group.next()
    print("next second: \(none)") // CHECK: next second: nil

    return (one ?? 0) + (none ?? 0)
  }

  print("result: \(result)") // CHECK: result: 1
}



@main struct Main {
  static func main() async {
    await test_taskGroup_cancel_then_add()
  }
}
