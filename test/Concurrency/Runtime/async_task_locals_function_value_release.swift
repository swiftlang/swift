// RUN: %target-run-simple-swift(-parse-as-library) | %FileCheck %s
// RUN: %target-run-simple-swift(-parse-as-library -O) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// In optimized builds specifically the task local add builtins (AddTaskLocalValue/TaskLocalValuePush)
// would end use a the AST type directly, which would then be reabstraction thunk with ~Copyable and
// other not representable at runtime types.

@TaskLocal var taskLocal: (() -> Void)?

@main
struct Main {
  static func main() {
    $taskLocal.withValue({}) {
      print("inside withValue") // CHECK: inside withValue
    }
    print("done") // CHECK: done
  }
}
