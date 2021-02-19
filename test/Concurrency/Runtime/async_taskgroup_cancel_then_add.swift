// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library) | %FileCheck %s --dump-input always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

import Dispatch

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

func asyncEcho(_ value: Int) async -> Int {
  value
}

func pprint(_ m: String, file: String = #file, line: UInt = #line) {
//  fputs("[\(file):\(line)] \(m)\n", stderr)
  print(m)
}

func test_taskGroup_cancel_then_add() async {
  // CHECK: test_taskGroup_cancel_then_add
  pprint("\(#function)")
  let result: Int = try! await Task.withGroup(resultType: Int.self) { group in

    let addedFirst = await group.add { 1 }
    pprint("added first: \(addedFirst)") // CHECK: added first: true

    let one = try! await group.next()!
    pprint("next first: \(one)") // CHECK: next first: 1

    group.cancelAll()
    pprint("cancelAll")

    let addedSecond = await group.add { 1 }
    pprint("added second: \(addedSecond)") // CHECK: added second: false

    let none = try! await group.next()
    pprint("next second: \(none)") // CHECK: next second: nil

    return (one ?? 0) + (none ?? 0)
  }

  pprint("result: \(result)") // CHECK: result: 1
}



@main struct Main {
  static func main() async {
    await test_taskGroup_cancel_then_add()
  }
}
