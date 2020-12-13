// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) | %FileCheck %s --dump-input always
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64

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
  _ = await try! Task.withGroup(resultType: Int.self) { (group) async -> Int in
    // CHECK: before add: isEmpty=true
    print("before add: isEmpty=\(group.isEmpty)")

    await group.add {
      sleep(2)
      return await asyncEcho(1)
    }

    // CHECK: while add running, outside: isEmpty=false
    print("while add running, outside: isEmpty=\(group.isEmpty)")

    // CHECK: next: 1
    while let value = await try! group.next() {
      print("next: \(value)")
    }

    // CHECK: after draining tasks: isEmpty=true
    print("after draining tasks: isEmpty=\(group.isEmpty)")
    return 0
  }
}

runAsyncAndBlock(test_taskGroup_isEmpty)
