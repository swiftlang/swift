// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Dispatch

@available(SwiftStdlib 5.5, *)
func test_taskGroup_cancel_from_inside_child() async {
  let one = try! await withTaskGroup(of: Int.self, returning: Int.self) { group in
    await group.next()
    return 0
  }

  let two = await withTaskGroup(of: Int.self, returning: Int.self) { group in
    0
  }

  let result = await withTaskGroup(of: Int.self, returning: Int.self) { group in
    let firstAdded = group.spawnUnlessCancelled { [group] in // must explicitly capture, as the task executes concurrently
      group.cancelAll() // allowed
      print("first")
      return 1
    }
    print("firstAdded: \(firstAdded)") // CHECK: firstAdded: true

    let one = await group.next()

    let secondAdded = group.spawnUnlessCancelled {
      print("second")
      return 2
    }
    print("secondAdded: \(secondAdded)") // CHECK: secondAdded: false

    return 1
  }

  print("result: \(result)") // CHECK: result: 1
}



@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    await test_taskGroup_cancel_from_inside_child()
  }
}
