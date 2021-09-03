// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library) | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: linux

#if os(Linux)
import Glibc
#elseif os(Windows)
import MSVCRT
#else
import Darwin
#endif

@available(SwiftStdlib 5.5, *)
func test_taskGroup_next() async {
  _ = await withTaskGroup(of: Int.self, returning: Int.self) { group in
    for n in 0..<100 {
      group.spawn {
        return n
      }
    }
    await Task.sleep(2_000_000)

    var sum = 0
    for await value in group {
      sum += 1
    }

    return sum
  }

  // CHECK: result with group.next(): 100
  print("result with group.next(): \(100)")
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    await test_taskGroup_next()
  }
}
