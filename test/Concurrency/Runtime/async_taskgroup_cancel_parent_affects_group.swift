// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library) | %FileCheck %s --dump-input always
// REQUIRES: executable_test
// REQUIRES: concurrency
// XFAIL: windows
// XFAIL: linux
// XFAIL: openbsd

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

func test_taskGroup_cancel_parent_affects_group() async {

  let x = Task.runDetached {
    try! await Task.withGroup(resultType: Int.self) { group -> Void in
      await group.add {
        sleep(3)
        let c = await Task.__unsafeCurrentAsync().isCancelled
        print("group task isCancelled: \(c)")
        return 0
      }

      _ = try! await group.next()
      let c = await Task.__unsafeCurrentAsync().isCancelled
      print("group isCancelled: \(c)")
    }
    let c = await Task.__unsafeCurrentAsync().isCancelled
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
