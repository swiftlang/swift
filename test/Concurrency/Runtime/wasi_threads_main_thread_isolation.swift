// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out pass 2>&1 | %FileCheck --check-prefix=PASS %s
// RUN: not --crash %target-run %t/a.out trap 2>&1 | %FileCheck --check-prefix=TRAP %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: OS=wasip1
// REQUIRES: wasi_threads

// The wasm32-unknown-wasip1-threads runtime is not compiled single-threaded,
// so the actor runtime can tell a pthread from the main thread: an isolation
// assumption holds on the main thread and traps on a worker. (With
// SWIFT_STDLIB_SINGLE_THREADED_CONCURRENCY every thread counts as the main
// thread and the worker's `assumeIsolated` passed silently.)

import WASILibc
import wasi_pthread

@MainActor func onMain() { print("main: isolated") }

func worker(_: UnsafeMutableRawPointer?) -> UnsafeMutableRawPointer? {
  // TRAP: Fatal error
  MainActor.assumeIsolated { print("worker: assumeIsolated passed (BUG)") }
  return nil
}

@main struct Main {
  static func main() {
    // PASS: main: isolated
    MainActor.assumeIsolated { onMain() }
    print("main: after")
    // PASS: main: after
    guard CommandLine.arguments.last == "trap" else { return }
    var thread = pthread_t(bitPattern: 0)
    let rc = pthread_create(&thread, nil, worker, nil)
    precondition(rc == 0, "pthread_create failed: \(rc)")
    if let thread { pthread_join(thread, nil) }
    print("worker returned")
  }
}
