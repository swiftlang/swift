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

func test_taskGroup_next() async {
  // CHECK: creating task [[MAIN_TASK:0x.*]] with parent 0x0
  // CHECK: creating task [[GROUP_TASK_1:0x.*]] with parent [[MAIN_TASK]]
  // CHECK: creating task [[GROUP_TASK_2:0x.*]] with parent [[MAIN_TASK]]
  // CHECK: creating task [[GROUP_TASK_3:0x.*]] with parent [[MAIN_TASK]]
  // CHECK: creating task [[GROUP_TASK_4:0x.*]] with parent [[MAIN_TASK]]
  // CHECK: creating task [[GROUP_TASK_5:0x.*]] with parent [[MAIN_TASK]]

  _ = await withTaskGroup(of: Int.self, returning: Int.self) { group in
    for n in 0..<5 {
      group.spawn {
        return n
      }
    }
    await Task.sleep(2_000_000)

    var sum = 0
    for await value in group {
      sum += 1
    }

    return sum
  }
  // as we exit the group, it must be guaranteed that its child tasks were destroyed
  //
  // NOTE: there is no great way to express "any of GROUP_TASK_n",
  //       so we just check that 5 tasks were destroyed
  //
  // CHECK: destroy task [[DESTROY_GROUP_TASK_1:0x.*]]
  // CHECK: destroy task [[DESTROY_GROUP_TASK_2:0x.*]]
  // CHECK: destroy task [[DESTROY_GROUP_TASK_3:0x.*]]
  // CHECK: destroy task [[DESTROY_GROUP_TASK_4:0x.*]]
  // CHECK: destroy task [[DESTROY_GROUP_TASK_5:0x.*]]
}

@main struct Main {
  static func main() async {
    await test_taskGroup_next()
  }
}
