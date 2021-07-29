// RUN: %target-swiftc_driver %s -Xfrontend -enable-experimental-concurrency -parse-as-library %import-libdispatch -target %sanitizers-target-triple -g -sanitize=thread -o %t
// RUN: %target-codesign %t
// RUN: env %env-TSAN_OPTIONS="abort_on_error=0" not %target-run %t 2>&1 | %swift-demangle --simplified | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch
// REQUIRES: tsan_runtime

// rdar://76038845
// UNSUPPORTED: use_os_stdlib

// rdar://80274830 ([Swift CI] Sanitizer report symbolication fails because we fail to start atos, sanbox issue?)
// REQUIRES: 80274830
// Might be related/same issue as below

// rdar://75365575 (Failing to start atos external symbolizer)
// UNSUPPORTED: OS=watchos

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

var racyCounter = 0

func asyncFib(_ n: Int) async -> Int {
  racyCounter += 1
  if n == 0 || n == 1 {
    return n
  }

  async let first = await asyncFib(n-2)
  async let second = await asyncFib(n-1)

  // Sleep a random amount of time waiting on the result producing a result.
  usleep(UInt32.random(in: 0..<100) * 1000)

  let result = await first + second

  // Sleep a random amount of time before producing a result.
  usleep(UInt32.random(in: 0..<100) * 1000)

  return result
}

func runFibonacci(_ n: Int) async {
  let result = await asyncFib(n)

  print()
  print("Async fib = \(result), sequential fib = \(fib(n))")
  assert(result == fib(n))
  print("asyncFib() called around \(racyCounter) times")
}

@main struct Main {
  static func main() async {
    await runFibonacci(10)
  }
}

// CHECK: ThreadSanitizer: Swift access race
// CHECK: Location is global 'racyCounter'
