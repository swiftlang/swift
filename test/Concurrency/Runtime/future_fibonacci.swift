// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency  %import-libdispatch)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

import Dispatch

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

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

func asyncFib(_ n: Int) async -> Int {
  if n == 0 || n == 1 {
    return n
  }

  let first = Task.runDetached {
    await asyncFib(n - 2)
  }

  let second = Task.runDetached {
    await asyncFib(n - 1)
  }

  // Sleep a random amount of time waiting on the result producing a result.
  usleep(UInt32.random(in: 0..<100) * 1000)

  let result = await try! first.get() + second.get()

  // Sleep a random amount of time before producing a result.
  usleep(UInt32.random(in: 0..<100) * 1000)

  return result
}

func runFibonacci(_ n: Int) {
  var result = 0
  runAsyncAndBlock {
    result = await asyncFib(n)
  }

  print()
  print("Async fib = \(result), sequential fib = \(fib(n))")
  assert(result == fib(n))
}

runFibonacci(15)
