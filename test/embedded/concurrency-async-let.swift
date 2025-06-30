// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-clang %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos -lswift_Concurrency -lswift_ConcurrencyDefaultExecutor -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

import _Concurrency

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

  let result = await first + second

  return result
}

@available(SwiftStdlib 5.1, *)
func runFibonacci(_ n: Int) async {
  let result = await asyncFib(n)

  print("")
  print(result == fib(n) ? "OK!" : "???")
  // CHECK: OK!
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await runFibonacci(10)
  }
}
