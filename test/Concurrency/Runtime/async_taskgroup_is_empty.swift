// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

import Dispatch

func asyncEcho(_ value: Int) async -> Int {
  value
}

func test_taskGroup_isEmpty() async {
  do {
    print("before all")
    let result = try await Task.withGroup(resultType: Int.self) {
      (group) async -> Int in
      // CHECK: before add: isEmpty=true
      print("before add: isEmpty=\(group.isEmpty)")

      await group.add {
        await Task.sleep(2_000_000_000)
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
      return 42
    }

    // CHECK: result: 42
    print("result: \(result)")
  } catch {
    fatalError("\(error)")
  }
}

@main struct Main {
  static func main() async {
    await test_taskGroup_isEmpty()
  }
}
