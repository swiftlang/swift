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
  fputs("[\(file):\(line)] \(m)\n", stderr)
  print(m)
}

/// Tests that only the specific group we cancelAll on is cancelled,
/// and not accidentally all tasks in all groups within the given parent task.
func test_taskGroup_cancelAll_onlySpecificGroup() async {
  async let g1: Int = Task.withGroup(resultType: Int.self) { group in

    for i in 1...5 {
      await group.add {
        sleep(1)
        let c = await Task.isCancelled()
        pprint("add: \(i) (cancelled: \(c))")
        return i
      }
    }

    var sum = 0
    while let got = try! await group.next() {
      pprint("next: \(got)")
      sum += got
    }

    let c = await Task.isCancelled()
    pprint("g1 task cancelled: \(c)")
    let cc = group.isCancelled
    pprint("g1 group cancelled: \(cc)")

    return sum
  }

  // The cancellation os g2 should have no impact on g1
  let g2: Int = try! await Task.withGroup(resultType: Int.self) { group in
    for i in 1...3 {
      await group.add {
        sleep(1)
        let c = await Task.isCancelled()
        pprint("g1 task \(i) (cancelled: \(c))")
        return i
      }
    }

    pprint("cancelAll")
    group.cancelAll()

    let c = await Task.isCancelled()
    pprint("g2 task cancelled: \(c)")
    let cc = group.isCancelled
    pprint("g2 group cancelled: \(cc)")
    return 0
  }

  let result1 = try! await g1
  let result2 = try! await g2

  // CHECK: g2 task cancelled: false
  // CHECK: g2 group cancelled: true
  // CHECK: g1 task cancelled: false
  // CHECK: g1 group cancelled: false

  pprint("g1: \(result1)") // CHECK: g1: 15
  pprint("g2: \(result2)") // CHECK: g2: 0
}



@main struct Main {
  static func main() async {
    await test_taskGroup_cancelAll_onlySpecificGroup()
  }
}
