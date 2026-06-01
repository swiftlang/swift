// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-swift-5.1-abi-triple -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: env %env-DYLD_LIBRARY_PATH=%swift-lib-dir/swift/%target-sdk-name %target-run %t/a.out
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

import _Concurrency
import Darwin
import Dispatch
import StdlibUnittest

// Resolve at runtime so this test links against the stock system dylib but
// can be pointed at our modified build via DYLD_LIBRARY_PATH.
// RTLD_DEFAULT (-2) searches all loaded images, unlike NULL which only searches
// the main program's namespace.
let RTLD_DEFAULT = UnsafeMutableRawPointer(bitPattern: -2)
func registryCount() -> Int {
  typealias Fn = @convention(c) () -> Int
  guard let sym = dlsym(RTLD_DEFAULT, "swift_task_registryCount") else { return -1 }
  return unsafeBitCast(sym, to: Fn.self)()
}

@main struct Main {
  static func main() async {
    let tests = TestSuite("TaskRegistry")

    // Spawn N tasks one at a time; verify the registry count rises by 1 after
    // each creation and falls by 1 after each task is fully destroyed.
    tests.test("perTaskIncrementAndDecrement") {
      let n = 20
      let baseline = registryCount()

      for i in 0..<n {
        let g = DispatchGroup()
        g.enter()

        let sem = DispatchSemaphore(value: 0)
        Task {
          sem.wait()
          g.leave()
        }

        let countAfterCreate = registryCount()
        expectGE(countAfterCreate, baseline + 1,
          "after spawning task \(i+1): expected count >= \(baseline + 1), got \(countAfterCreate)")

        sem.signal()
        g.wait()

        let countAfterDestroy = registryCount()
        expectLE(countAfterDestroy, baseline,
          "after task \(i+1) finished: expected count <= \(baseline), got \(countAfterDestroy)")
      }
    }

    await runAllTestsAsync()
  }
}
