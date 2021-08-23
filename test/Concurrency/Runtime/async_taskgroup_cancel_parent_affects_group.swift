// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Dispatch

@available(SwiftStdlib 5.5, *)
func asyncEcho(_ value: Int) async -> Int {
  value
}

@available(SwiftStdlib 5.5, *)
func test_taskGroup_cancel_parent_affects_group() async {

  let x = detach {
    await withTaskGroup(of: Int.self, returning: Void.self) { group in
      group.spawn {
        await Task.sleep(3_000_000_000)
        let c = Task.isCancelled
        print("group task isCancelled: \(c)")
        return 0
      }

      _ = await group.next()
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



@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    await test_taskGroup_cancel_parent_affects_group()
  }
}
