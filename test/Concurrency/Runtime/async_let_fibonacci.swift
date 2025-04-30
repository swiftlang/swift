// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: freestanding

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

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

@available(SwiftStdlib 5.1, *)
func asyncFib(_ n: Int) async -> Int {
  if n == 0 || n == 1 {
    return n
  }

  async let first = await asyncFib(n-2)
  async let second = await asyncFib(n-1)

  // Sleep a random amount of time waiting on the result producing a result.
  try! await Task.sleep(nanoseconds: UInt64.random(in: 0..<100) * 1_000_000)

  let result = await first + second

  // Sleep a random amount of time before producing a result.
  try! await Task.sleep(nanoseconds: UInt64.random(in: 0..<100) * 1_000_000)

  return result
}

@available(SwiftStdlib 5.1, *)
func runFibonacci(_ n: Int) async {
  let result = await asyncFib(n)

  print("")
  print("Async fib = \(result), sequential fib = \(fib(n))")
  assert(result == fib(n))
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await runFibonacci(10)
  }
}
