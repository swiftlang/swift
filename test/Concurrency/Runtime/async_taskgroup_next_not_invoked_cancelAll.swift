// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

import Dispatch

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

func test_skipCallingNext_butInvokeCancelAll() async {
  let numbers = [1, 1]

  let result = try! await Task.withGroup(resultType: Int.self) { (group) async -> Int in
    for n in numbers {
      print("group.add { \(n) }")
      await group.add { () async -> Int in
        sleep(2)
        print("  inside group.add { \(n) }")
        let c = await Task.__unsafeCurrentAsync().isCancelled
        print("  inside group.add { \(n) } (task cancelled: \(c))")
        return n
      }
    }

    group.cancelAll()

    // return immediately; the group should wait on the tasks anyway
    let c = await Task.__unsafeCurrentAsync().isCancelled
    print("return immediately 0 (task cancelled: \(c))")
    print("return immediately 0 (group cancelled: \(group.isCancelled))")
    return 0
  }

  // CHECK: group.add { 1 }
  // CHECK: group.add { 1 }
  // CHECK: return immediately 0 (task cancelled: false)
  // CHECK: return immediately 0 (group cancelled: true)
  // CHECK: inside group.add { 1 } (task cancelled: true)
  // CHECK: inside group.add { 1 } (task cancelled: true)

  // CHECK: result: 0
  print("result: \(result)")
  assert(result == 0)
}

@main struct Main {
  static func main() async {
    await test_skipCallingNext_butInvokeCancelAll()
  }
}
