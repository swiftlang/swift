// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library) | %FileCheck %s --dump-input always
// REQUIRES: executable_test
// REQUIRES: concurrency

import func Foundation.sleep

func asyncEcho(_ value: Int) async -> Int {
  value
}

func pprint(_ m: String, file: String = #file, line: UInt = #line) {
//  fputs("[\(file):\(line)] \(m)\n", stderr)
  print(m)
}

func test_taskGroup_cancel_then_completions() async {
  // CHECK: test_taskGroup_cancel_then_completions
  pprint("before \(#function)")

//  async let outer: Bool = {
//    sleep(6)
//    return await Task.__unsafeCurrentAsync().isCancelled
//  }()

  let result: Int = try! await Task.withGroup(resultType: (Int, Bool).self) { group in
    pprint("group cancelled: \(group.isCancelled)") // CHECK: group cancelled: false
    let addedFirst = await group.add {
      pprint("start first")
      sleep(1)
      pprint("done first")
      return (1, await Task.__unsafeCurrentAsync().isCancelled)
    }
    pprint("added first: \(addedFirst)") // CHECK: added first: true
    assert(addedFirst)

    let addedSecond = await group.add {
      pprint("start second")
      sleep(3)
      pprint("done second")
      return (2, await Task.__unsafeCurrentAsync().isCancelled)
    }
    pprint("added second: \(addedSecond)") // CHECK: added second: true
    assert(addedSecond)

    group.cancelAll() // FIXME: dont make it async
    pprint("cancelAll") // CHECK: cancelAll

//    let outerCancelled = await outer // should not be cancelled
//    pprint("outer cancelled: \(outerCancelled)") // COM: CHECK: outer cancelled: false
//    pprint("group cancelled: \(group.isCancelled)") // COM: CHECK: outer cancelled: false

    let one = try! await group.next()
    pprint("first: \(one)") // CHECK: first: Optional((1,
    let two = try! await group.next()
    pprint("second: \(two)") // CHECK: second: Optional((2,
    let none = try! await group.next()
    pprint("none: \(none)") // CHECK: none: nil

    return (one?.0 ?? 0) + (two?.0 ?? 0) + (none?.0 ?? 0)
  }

  pprint("result: \(result)") // CHECK: result: 3
}

@main struct Main {
  static func main() async {
//    await test_taskGroup_cancel_then_add()
    await test_taskGroup_cancel_then_completions()
  }
}
