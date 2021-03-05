// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

import Dispatch

func asyncEcho(_ value: Int) async -> Int {
  value
}

func test_taskGroup_cancel_parent_affects_group() async {

  let x = Task.runDetached {
    try! await Task.withGroup(resultType: Int.self) { group -> Void in
      await group.add {
        await Task.sleep(3_000_000_000)
        let c = Task.isCancelled
        print("group task isCancelled: \(c)")
        return 0
      }

      _ = try! await group.next()
      let c = Task.isCancelled
      print("group isCancelled: \(c)")
    }
    let c = Task.isCancelled
    print("detached task isCancelled: \(c)")
  }

  x.cancel()
  try! await x.get()

  // CHECK: group task isCancelled: true
  // CHECK: group isCancelled: true
  // CHECK: detached task isCancelled: true
  // CHECK: done
  print("done")
}



@main struct Main {
  static func main() async {
    await test_taskGroup_cancel_parent_affects_group()
  }
}
