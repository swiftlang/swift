// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

import Dispatch

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

extension DispatchQueue {
  func async<R>(execute: @escaping () async throws -> R) -> Task.Handle<R> {
    let handle = Task.runDetached(operation: execute)

    // Run the task
    _ = { self.async { handle.run() } }()

    return handle
  }

  func async<R>(in group: DispatchGroup,
    execute: @escaping () async throws -> R) -> Task.Handle<R> {
    let handle = Task.runDetached(operation: execute)

    // Run the task
    group.enter()
    _ = {
      self.async {
        handle.run()
        group.leave()
      }
    }()

    return handle
  }
}

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

func asyncFib(_ n: Int, group: DispatchGroup, queue: DispatchQueue) async -> Int {
  if n == 0 || n == 1 {
    return n
  }

  let first = queue.async(in: group) {
    await asyncFib(n - 2, group: group, queue: queue)
  }

  let second = queue.async(in: group) {
    await asyncFib(n - 1, group: group, queue: queue)
  }

  // Sleep a random amount of time waiting on the result producing a result.
  usleep(UInt32.random(in: 0..<100) * 1000)

  let result = await try! first.get() + second.get()

  // Sleep a random amount of time before producing a result.
  usleep(UInt32.random(in: 0..<100) * 1000)

  return result
}

func runFibonacci(_ n: Int) {
  let queue = DispatchQueue(label: "concurrent", attributes: .concurrent)
  let group = DispatchGroup()

  var result = 0
  _ = queue.async(in: group) {
    result = await asyncFib(n, group: group, queue: queue)
  }
  group.wait()

  print()
  print("Async fib = \(result), sequential fib = \(fib(n))")
  assert(result == fib(n))
}

runFibonacci(15)
