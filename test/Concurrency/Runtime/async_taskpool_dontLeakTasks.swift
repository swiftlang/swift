// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library) 2>&1 | %FileCheck %s --dump-input=always
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: swift_task_debug_log

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

#if os(Linux)
import Glibc
#elseif os(Windows)
import MSVCRT
#else
import Darwin
#endif

func test_taskPool_next() async {
  // CHECK: creating task [[MAIN_TASK:0x.*]] with parent 0x0

  _ = await withTaskPool(returning: Int.self) { pool in
    for n in 0..<5 {
      pool.addTask {
        print("run \(n))")
      }
    }
    return 0
  }
  // as we exit the pool, it must be guaranteed that its child tasks were destroyed
  //
  // NOTE: there is no great way to express "any of POOL_TASK_n",
  //       so we just check that 5 tasks were destroyed
  //
  // CHECK: destroy task [[DESTROY_POOL_TASK_1:0x.*]]
  // CHECK: destroy task [[DESTROY_POOL_TASK_2:0x.*]]
  // CHECK: destroy task [[DESTROY_POOL_TASK_3:0x.*]]
  // CHECK: destroy task [[DESTROY_POOL_TASK_4:0x.*]]
  // CHECK: destroy task [[DESTROY_POOL_TASK_5:0x.*]]
}

@main struct Main {
  static func main() async {
    await test_taskPool_next()
  }
}
