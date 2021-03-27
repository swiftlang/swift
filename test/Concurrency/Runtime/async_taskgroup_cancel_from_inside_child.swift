// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

import Dispatch

func test_taskGroup_cancel_from_inside_child() async {
  let result: Int = try! await Task.withGroup(resultType: Int.self) { group in
    let firstAdded = await group.add { [group] in // must explicitly capture, as the task executes concurrently
      group.cancelAll() // allowed
      print("first")
      return 1
    }
    print("firstAdded: \(firstAdded)") // CHECK: firstAdded: true

    let one = try! await group.next()

    let secondAdded = await group.add {
      print("second")
      return 2
    }
    print("secondAdded: \(secondAdded)") // CHECK: secondAdded: false

    return 1
  }

  print("result: \(result)") // CHECK: result: 1
}



@main struct Main {
  static func main() async {
    await test_taskGroup_cancel_from_inside_child()
  }
}
