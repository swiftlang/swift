// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: not --crash %target-run %t/a.out 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: OS=wasip1
// REQUIRES: wasi_threads

import WASILibc
import wasi_pthread

func worker(_: UnsafeMutableRawPointer?) -> UnsafeMutableRawPointer? {
  MainActor.assumeIsolated { print("OK") }
  return nil
}

@main struct Main {
  static func main() {
    MainActor.assumeIsolated { print("main") }
    var thread = pthread_t(bitPattern: 0)
    guard pthread_create(&thread, nil, worker, nil) == 0 else { return }
    pthread_join(thread!, nil)
  }
}

// CHECK: main
// CHECK: Incorrect actor executor assumption
// CHECK-NOT: OK
