// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library) | %FileCheck %s
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

func test_taskGroup_isEmpty() async {
  _ = try! await Task.withGroup(resultType: Int.self) { (group) async -> Int in
    // CHECK: before add: isEmpty=true
    print("before add: isEmpty=\(group.isEmpty)")

    await group.add {
      sleep(2)
      return await asyncEcho(1)
    }

    // CHECK: while add running, outside: isEmpty=false
    print("while add running, outside: isEmpty=\(group.isEmpty)")

    // CHECK: next: 1
    while let value = try! await group.next() {
      print("next: \(value)")
    }

    // CHECK: after draining tasks: isEmpty=true
    print("after draining tasks: isEmpty=\(group.isEmpty)")
    return 0
  }
}

@main struct Main {
  static func main() async {
    await test_taskGroup_isEmpty()
  }
}
