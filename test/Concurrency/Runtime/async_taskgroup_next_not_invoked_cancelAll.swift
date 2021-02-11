// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library) | %FileCheck %s --dump-input=always
// REQUIRES: executable_test
// REQUIRES: concurrency
// XFAIL: windows
// XFAIL: linux
// XFAIL: openbsd

import Dispatch

func test_skipCallingNext_butInvokeCancelAll() async {
  let numbers = [1, 1]

  let result = try! await Task.withGroup(resultType: Int.self) { (group) async -> Int in
    for n in numbers {
      print("group.add { \(n) }")
      await group.add { () async -> Int in
        sleep(1)
        print("  inside group.add { \(n) }")
        let cancelled = await Task.__unsafeCurrentAsync().isCancelled
        print("  inside group.add { \(n) } (canceled: \(cancelled))")
        return n
      }
    }

    group.cancelAll()

    // return immediately; the group should wait on the tasks anyway
    let c = await Task.__unsafeCurrentAsync().isCancelled
    print("return immediately 0 (canceled: \(c))")
    return 0
  }

  // CHECK: group.add { 1 }
  // CHECK: group.add { 1 }
  // CHECK: return immediately 0 (canceled: true)

  // CHECK: inside group.add { 1 }
  // CON: inside group.add { 1 } (canceled: true) // TODO: Actually the child tasks should become cancelled as well, but that's not implemented yet

  // CHECK: inside group.add { 1 }
  // CON: inside group.add { 1 } (canceled: true) // TODO: Actually the child tasks should become cancelled as well, but that's not implemented yet

  // CHECK: result: 0
  print("result: \(result)")
  assert(result == 0)
}

@main struct Main {
  static func main() async {
    await test_skipCallingNext_butInvokeCancelAll()
  }
}
