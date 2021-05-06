// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency  %import-libdispatch -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Dispatch

func fib(_ n: Int) -> Int {
    var first = 0
    var second = 1
    for _ in 0..<n {
        let temp = first
        first = second
        second = temp + first
    }
    return first
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func asyncFib(_ n: Int) async -> Int {
  if n == 0 || n == 1 {
    return n
  }

  let first = detach {
    await asyncFib(n - 2)
  }

  let second = detach {
    await asyncFib(n - 1)
  }

  // Sleep a random amount of time waiting on the result producing a result.
  await Task.sleep(UInt64.random(in: 0..<100) * 1_000_000)

  let result = try! await first.get() + second.get()

  // Sleep a random amount of time before producing a result.
  await Task.sleep(UInt64.random(in: 0..<100) * 1_000_000)

  return result
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func runFibonacci(_ n: Int) async {
  var result = await asyncFib(n)

  print()
  print("Async fib = \(result), sequential fib = \(fib(n))")
  assert(result == fib(n))
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@main struct Main {
  static func main() async {
    await runFibonacci(15)
  }
}
