// RUN: %target-run-simple-swift( -O -target %target-swift-5.1-abi-triple -parse-as-library) 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// Ensure that the CheckedContinuation's function string isn't destroyed before
// the diagnostic is printed.

import _Concurrency

@inline(never)
func heapAllocatedName() -> String {
  // Long enough to require heap storage. The random component ensures that it
  // can't be a static string.
  return "0123456789abcdef\(Int.random(in: 0...1))"
}

@main struct Main {
  static func main() async {
    for _ in 0..<50 {
      await withUnsafeContinuation { (uc: UnsafeContinuation<Void, Never>) in
        do {
          let checked = unsafe CheckedContinuation(continuation: uc,
                                                  function: heapAllocatedName())
          _ = checked
        }
        unsafe uc.resume()
      }
    }

    print("done")
  }
}

// CHECK-COUNT-50: SWIFT TASK CONTINUATION MISUSE: 0123456789abcdef{{0|1}} leaked its continuation
// CHECK: done
