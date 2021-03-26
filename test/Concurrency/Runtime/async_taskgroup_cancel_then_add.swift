// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib

import Dispatch

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func asyncEcho(_ value: Int) async -> Int {
  value
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func test_taskGroup_cancel_then_add() async {
  // CHECK: test_taskGroup_cancel_then_add
  print("\(#function)")
  let result: Int = await withTaskGroup(of: Int.self) { group in

    let addedFirst = await group.spawn { 1 }
    print("added first: \(addedFirst)") // CHECK: added first: true

    let one = await group.next()!
    print("next first: \(one)") // CHECK: next first: 1

    group.cancelAll()
    print("cancelAll")

    let addedSecond = await group.spawn { 1 }
    print("added second: \(addedSecond)") // CHECK: added second: false

    let none = await group.next()
    print("next second: \(none)") // CHECK: next second: nil

    return one + (none ?? 0)
  }

  print("result: \(result)") // CHECK: result: 1
}



@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@main struct Main {
  static func main() async {
    await test_taskGroup_cancel_then_add()
  }
}
