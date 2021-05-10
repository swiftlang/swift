// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Dispatch

@available(SwiftStdlib 5.5, *)
func test_skipCallingNext_butInvokeCancelAll() async {
  let numbers = [1, 1]

  let result = try! await withTaskGroup(of: Int.self) { (group) async -> Int in
    for n in numbers {
      print("group.spawn { \(n) }")
      group.spawn { [group] () async -> Int in
        await Task.sleep(1_000_000_000)
        print("  inside group.spawn { \(n) }")
        print("  inside group.spawn { \(n) } (group cancelled: \(group.isCancelled))")
        print("  inside group.spawn { \(n) } (group child task cancelled: \(Task.isCancelled))")
        return n
      }
    }

    group.cancelAll()

    // return immediately; the group should wait on the tasks anyway
    print("return immediately 0 (group cancelled: \(group.isCancelled))")
    print("return immediately 0 (task cancelled: \(Task.isCancelled))")
    return 0
  }

  // CHECK: group.spawn { 1 }
  //
  // CHECK: return immediately 0 (group cancelled: true)
  // CHECK: return immediately 0 (task cancelled: false)
  //
  // CHECK: inside group.spawn { 1 } (group cancelled: true)
  // CHECK: inside group.spawn { 1 } (group child task cancelled: true)

  // CHECK: result: 0
  print("result: \(result)")
  assert(result == 0)
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    await test_skipCallingNext_butInvokeCancelAll()
  }
}
