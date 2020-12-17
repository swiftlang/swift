// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) | %FileCheck %s --dump-input=always
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64

import func Foundation.sleep

func test_skipCallingNext() async {
  let numbers = [1, 1]

  let result = await try! Task.withGroup(resultType: Int.self) { (group) async -> Int in
    for n in numbers {
      print("group.add { \(n) }")
      await group.add { () async -> Int in
        sleep(1)
        print("  inside group.add { \(n) } (canceled: \(await Task.isCancelled()))")
        return n
      }
    }

    // return immediately; the group should wait on the tasks anyway
    print("return immediately 0 (canceled: \(await Task.isCancelled()))")
    return 0
  }

  // CHECK: group.add { 1 }
  // CHECK: group.add { 1 }
  // CHECK: return immediately 0 (canceled: false)

  // CHECK: inside group.add { 1 } (canceled: false)
  // CHECK: inside group.add { 1 } (canceled: false)

  // CHECK: result: 0
  print("result: \(result)")
  assert(result == 0)
}

runAsyncAndBlock(test_skipCallingNext)

