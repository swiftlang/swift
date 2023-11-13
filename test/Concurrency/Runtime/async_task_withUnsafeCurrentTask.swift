// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library) 2>&1 | %FileCheck %s --dump-input=always
// REQUIRES: executable_test
// REQUIRES: concurrency

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

#if os(Linux)
import Glibc
#elseif os(Windows)
import MSVCRT
#else
import Darwin
#endif

func test_withUnsafeCurrentTask() async {
  // The task we're running in ("main")
  let t = Task.detached {
    _ = withUnsafeCurrentTask { task in
      fputs("OK: \(task!)\n", stderr)
    }
    fputs("DONE", stderr)
  }

  // CHECK: OK: UnsafeCurrentTask(_task: (Opaque Value))
  // CHECK: DONE
  await t.value
}

@main struct Main {
  static func main() async {
    await test_withUnsafeCurrentTask()
  }
}
