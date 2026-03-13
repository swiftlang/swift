// RUN: %target-swiftc_driver %s -Xfrontend -disable-availability-checking -parse-as-library %import-libdispatch -g -sanitize=thread -o %t
// RUN: %target-codesign %t
// RUN: env %env-TSAN_OPTIONS="abort_on_error=0" not %target-run %t 2>&1 | %swift-demangle --simplified | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch
// REQUIRES: tsan_runtime

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// rdar://86825277
// https://github.com/apple/swift/issues/58082
// UNSUPPORTED: CPU=arm64 || CPU=arm64e

// Disabled because this test is flaky rdar://76542113
// REQUIRES: rdar76542113

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

var racyCounter = 0

@available(SwiftStdlib 5.1, *)
func asyncFib(_ n: Int) async -> Int {
  racyCounter += 1
  if n == 0 || n == 1 {
    return n
  }

  async let first = await asyncFib(n-2)
  async let second = await asyncFib(n-1)

  // Sleep a random amount of time waiting on the result producing a result.
  await Task.sleep(UInt64.random(in: 0..<100) * 1_000_000)

  let result = await first + second

  // Sleep a random amount of time before producing a result.
  await Task.sleep(UInt64.random(in: 0..<100) * 1_000_000)

  return result
}

@available(SwiftStdlib 5.1, *)
func runFibonacci(_ n: Int) async {
  let result = await asyncFib(n)

  print()
  print("Async fib = \(result), sequential fib = \(fib(n))")
  assert(result == fib(n))
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await runFibonacci(10)
  }
}

// CHECK: ThreadSanitizer: Swift access race
// CHECK: Location is global 'racyCounter'
